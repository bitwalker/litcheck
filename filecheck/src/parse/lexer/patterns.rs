use std::borrow::Cow;

use either::{
    Either,
    Either::{Left, Right},
};

#[derive(Debug)]
pub struct Pattern {
    pub raw: String,
    pub prefix: Prefix,
    pub ty: Check,
}
impl Pattern {
    fn new(prefixes: &str, config: Either<Check, (Check, &str)>) -> Self {
        const WORD_BOUNDARY: &str = r#"(?-u:\b)"#;
        const MODIFIERS: &str = r#"([{](?<modifiers>LITERAL)[}])?"#;

        match config {
            Left(Check::Comment) => {
                let raw = format!("(?m){WORD_BOUNDARY}(?<prefix>{prefixes}):(?<comment>.*)$");
                Self::comment(raw)
            }
            Left(ty) => {
                let label = ty.suffix();
                let raw = format!("{WORD_BOUNDARY}(?<prefix>{prefixes}){label}{MODIFIERS}:");
                Self::check(raw, ty)
            }
            Right((ty, label)) => {
                let raw = format!("{WORD_BOUNDARY}(?<prefix>{prefixes}){label}{MODIFIERS}:");
                Self::check(raw, ty)
            }
        }
    }

    pub fn check(pattern: String, ty: Check) -> Self {
        Self {
            raw: pattern,
            prefix: Prefix::Check,
            ty,
        }
    }

    pub fn comment(pattern: String) -> Self {
        Self {
            raw: pattern,
            prefix: Prefix::Comment,
            ty: Check::Comment,
        }
    }

    pub fn generate_check_patterns(prefixes: &[Box<str>]) -> impl Iterator<Item = Pattern> + '_ {
        PatternGen::new(Prefix::Check, prefixes)
    }

    pub fn generate_comment_patterns(prefixes: &[Box<str>]) -> impl Iterator<Item = Pattern> + '_ {
        PatternGen::new(Prefix::Comment, prefixes)
    }
}
impl AsRef<str> for Pattern {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.raw.as_str()
    }
}

struct PatternGen<'a> {
    prefixes: Cow<'a, str>,
    checks: &'static [Check],
}
impl<'a> PatternGen<'a> {
    fn new(prefix_ty: Prefix, prefixes: &[Box<str>]) -> Self {
        let prefix_alt = if prefixes.is_empty() {
            match prefix_ty {
                Prefix::Check => Cow::Borrowed("CHECK"),
                Prefix::Comment => Cow::Borrowed("COM|RUN"),
            }
        } else {
            let mut alt = String::new();
            for (i, prefix) in prefixes.iter().enumerate() {
                if i > 0 {
                    alt.push('|');
                }
                alt.push_str(prefix);
            }
            Cow::Owned(alt)
        };
        Self {
            prefixes: prefix_alt,
            checks: match prefix_ty {
                Prefix::Check => Check::CHECKS,
                Prefix::Comment => &[Check::Comment],
            },
        }
    }
}
impl<'a> Iterator for PatternGen<'a> {
    type Item = Pattern;

    fn next(&mut self) -> Option<Self::Item> {
        let (check, rest) = self.checks.split_first()?;
        self.checks = rest;
        let config = match check {
            Check::Count => Right((Check::Count, r"-COUNT-(?<count>[0-9]+)")),
            check => Left(*check),
        };
        Some(Pattern::new(&self.prefixes, config))
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Prefix {
    Check,
    Comment,
}

#[derive(Debug, Copy, Clone)]
pub enum Check {
    Label,
    Plain,
    Empty,
    Next,
    Same,
    Not,
    Dag,
    Count,
    Comment,
}
impl Check {
    const CHECKS: &'static [Self] = &[
        Check::Count,
        Check::Empty,
        Check::Label,
        Check::Plain,
        Check::Next,
        Check::Same,
        Check::Dag,
        Check::Not,
    ];

    pub fn suffix(&self) -> &'static str {
        match self {
            Self::Plain => "",
            Self::Next => "-NEXT",
            Self::Same => "-SAME",
            Self::Not => "-NOT",
            Self::Dag => "-DAG",
            Self::Label => "-LABEL",
            Self::Empty => "-EMPTY",
            Self::Count => "-COUNT",
            Self::Comment => "",
        }
    }
}
impl TryFrom<Check> for crate::ast::Check {
    type Error = ();

    fn try_from(check: Check) -> Result<Self, Self::Error> {
        match check {
            Check::Plain => Ok(Self::Plain),
            Check::Next => Ok(Self::Next),
            Check::Same => Ok(Self::Same),
            Check::Not => Ok(Self::Not),
            Check::Dag => Ok(Self::Dag),
            Check::Label => Ok(Self::Label),
            Check::Empty => Ok(Self::Empty),
            Check::Count | Check::Comment => Err(()),
        }
    }
}
