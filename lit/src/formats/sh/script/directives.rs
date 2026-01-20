use std::{any::Any, fmt, str::FromStr};

use litcheck::diagnostics::{Diagnostic, SourceId, SourceSpan, Span, Spanned};
use regex::Regex;

use crate::config::ScopedSubstitutionSet;

thread_local! {
    static KEYWORD_RE: Regex = Regex::new("(DEFINE:|END\\.|REDEFINE:|REQUIRES:|RUN:|UNSUPPORTED:|XFAIL:|ALLOWED_RETRIES:)(.*)$").unwrap();
    static SUBSTITUTION_NAME_RE: Regex = Regex::new(r"^%\{[_a-zA-Z][-_:0-9a-zA-Z]*\}$").unwrap();
}

#[derive(Debug, Clone)]
pub struct RawDirective<'a> {
    pub kind: DirectiveKind,
    pub content: &'a str,
}
impl<'a> RawDirective<'a> {
    pub fn parse(source_id: SourceId, input: Span<&'a str>) -> Option<Span<Self>> {
        let start = input.span().start().to_usize();
        let captures = KEYWORD_RE.with(|re| re.captures(input.into_inner()))?;
        let range = captures.get(0).unwrap().range();
        let (_, [keyword, content]) = captures.extract();
        let kind = keyword.parse::<DirectiveKind>().unwrap();
        let span =
            SourceSpan::from_range_unchecked(source_id, (range.start + start)..(range.end + start));
        Some(Span::new(span, Self { kind, content }))
    }
}

#[derive(Debug)]
pub struct InvalidDirectiveKindError;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DirectiveKind {
    Run,
    Define,
    Redefine,
    Unsupported,
    Requires,
    Xfail,
    AllowedRetries,
    End,
}
impl DirectiveKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Run => "RUN:",
            Self::Define => "DEFINE:",
            Self::Redefine => "REDEFINE:",
            Self::Unsupported => "UNSUPPORTED:",
            Self::Requires => "REQUIRES:",
            Self::Xfail => "XFAIL:",
            Self::AllowedRetries => "ALLOWED_RETRIES:",
            Self::End => "END.",
        }
    }
}
impl FromStr for DirectiveKind {
    type Err = InvalidDirectiveKindError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "END." => Ok(Self::End),
            "RUN:" => Ok(Self::Run),
            "DEFINE:" => Ok(Self::Define),
            "REDEFINE:" => Ok(Self::Redefine),
            "REQUIRES:" => Ok(Self::Requires),
            "UNSUPPORTED:" => Ok(Self::Unsupported),
            "XFAIL:" => Ok(Self::Xfail),
            "ALLOWED_RETRIES:" => Ok(Self::AllowedRetries),
            _ => Err(InvalidDirectiveKindError),
        }
    }
}

impl fmt::Display for DirectiveKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

pub trait Directive: Any + fmt::Debug + Spanned {
    #[allow(unused)]
    fn kind(&self) -> DirectiveKind;
    fn line(&self) -> usize;
}

#[derive(Debug)]
pub enum ScriptCommand {
    Run(Run),
    Substitution(Substitution),
}
impl ScriptCommand {
    #[cfg(test)]
    pub fn unwrap_run(&self) -> &Run {
        match self {
            Self::Run(run) => run,
            other => panic!("expected a RUN: directive, but got a: {other:?}"),
        }
    }

    #[cfg(test)]
    pub fn unwrap_sub(&self) -> &Substitution {
        match self {
            Self::Substitution(sub) => sub,
            other => panic!("expected a DEFINE: or REDEFINE: directive, but got a: {other:?}"),
        }
    }
}
impl Spanned for ScriptCommand {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Run(ref command) => command.span(),
            Self::Substitution(ref command) => command.span(),
        }
    }
}
impl Directive for ScriptCommand {
    fn line(&self) -> usize {
        match self {
            Self::Run(ref directive) => directive.line(),
            Self::Substitution(ref directive) => directive.line(),
        }
    }
    fn kind(&self) -> DirectiveKind {
        match self {
            Self::Run(ref directive) => directive.kind(),
            Self::Substitution(ref directive) => directive.kind(),
        }
    }
}

#[derive(Debug)]
pub struct Run {
    pub span: SourceSpan,
    pub line: usize,
    pub command: String,
}
impl Spanned for Run {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl Directive for Run {
    fn line(&self) -> usize {
        self.line
    }
    fn kind(&self) -> DirectiveKind {
        DirectiveKind::Run
    }
}

#[derive(Diagnostic, Debug, thiserror::Error)]
pub enum InvalidSubstitutionError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidPattern(#[from] crate::config::InvalidSubstitutionPatternError),
    #[error("invalid substitution pattern")]
    #[diagnostic()]
    InvalidKey {
        #[label("'{key}' is not a valid pattern in DEFINE or REDEFINE directives")]
        span: SourceSpan,
        key: String,
    },
    #[error("invalid REDFINE directive")]
    #[diagnostic()]
    NotDefined {
        #[label("'{key}' is not defined")]
        span: SourceSpan,
        key: String,
    },
    #[error("invalid DEFINE directive")]
    #[diagnostic()]
    AlreadyExists {
        #[label("'{key}' is already defined")]
        span: SourceSpan,
        key: String,
    },
    #[error("substitution conflict")]
    #[diagnostic()]
    Conflicts {
        #[label("'{key}' conflicts with a previous substitution pattern '{pattern}'")]
        span: SourceSpan,
        key: String,
        pattern: String,
    },
}

#[derive(Debug)]
pub struct Substitution {
    pub span: SourceSpan,
    pub line: usize,
    pub key: String,
    pub value: String,
    pub replace: bool,
}
impl Substitution {
    pub fn new(
        span: SourceSpan,
        line: usize,
        key: &str,
        value: &str,
        replace: bool,
    ) -> Result<Self, InvalidSubstitutionError> {
        if !SUBSTITUTION_NAME_RE.with(|re| re.is_match(key)) {
            return Err(InvalidSubstitutionError::InvalidKey {
                span,
                key: key.to_string(),
            });
        }

        Ok(Self {
            span,
            line,
            key: key.to_string(),
            value: value.to_string(),
            replace,
        })
    }
}
impl Spanned for Substitution {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl Directive for Substitution {
    fn line(&self) -> usize {
        self.line
    }
    fn kind(&self) -> DirectiveKind {
        if self.replace {
            DirectiveKind::Redefine
        } else {
            DirectiveKind::Define
        }
    }
}
impl Substitution {
    pub fn apply(
        &self,
        substitutions: &mut ScopedSubstitutionSet<'_>,
    ) -> Result<(), InvalidSubstitutionError> {
        use crate::config::substitutions::Matches;
        use itertools::Itertools;

        match substitutions.find_matching(&self.key) {
            Matches::Empty if !self.replace => {
                substitutions.insert(self.key.clone(), self.value.clone());
                Ok(())
            }
            Matches::Empty => Err(InvalidSubstitutionError::NotDefined {
                span: self.span,
                key: self.key.clone(),
            }),
            Matches::Exact(_) if self.replace => {
                substitutions.insert(self.key.clone(), self.value.clone());
                Ok(())
            }
            Matches::Exact(_) => Err(InvalidSubstitutionError::AlreadyExists {
                span: self.span,
                key: self.key.clone(),
            }),
            #[allow(unstable_name_collisions)] // For intersperse: remove when stable
            matches => {
                let existing = matches.intersperse(", ").collect::<String>();
                Err(InvalidSubstitutionError::Conflicts {
                    span: self.span,
                    key: self.key.clone(),
                    pattern: existing,
                })
            }
        }
    }
}
