use std::cmp::Ordering;

use crate::{
    common::*,
    expr::{TypedVariable, ValueType},
};

#[derive(Debug, Copy, Clone)]
pub enum Capture {
    /// Ignore the capture
    Ignore(SourceSpan),
    /// Capture the entire match, but without a name
    All(Span<ValueType>),
    /// Capture the entire match, and bind it with the given name and type
    Implicit(TypedVariable),
    /// Capture a specific named group, and bind it with a different name and type
    Mapped { group: Symbol, with: TypedVariable },
    /// Capture a specific group with the given name and type
    Explicit(TypedVariable),
}
impl Default for Capture {
    #[inline(always)]
    fn default() -> Self {
        Self::Ignore(SourceSpan::UNKNOWN)
    }
}
impl Eq for Capture {}
impl PartialEq for Capture {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ignore(_), Self::Ignore(_)) => true,
            (Self::All(_), Self::All(_)) => true,
            (Self::Implicit(l), Self::Implicit(r)) => l == r,
            (Self::Mapped { group: gl, with: l }, Self::Mapped { group: gr, with: r }) => {
                gl == gr && l == r
            }
            (Self::Explicit(l), Self::Explicit(r)) => l == r,
            _ => false,
        }
    }
}
impl PartialOrd for Capture {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Capture {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        match (self, other) {
            (Self::Ignore(_), Self::Ignore(_)) => Ordering::Equal,
            (Self::Ignore(_), _) => Ordering::Less,
            (_, Self::Ignore(_)) => Ordering::Greater,
            (Self::All(_), Self::All(_)) => Ordering::Equal,
            (Self::All(_), _) => Ordering::Less,
            (_, Self::All(_)) => Ordering::Greater,
            (Self::Implicit(l), Self::Implicit(r)) => l.cmp(r),
            (Self::Implicit(_), _) => Ordering::Less,
            (_, Self::Implicit(_)) => Ordering::Greater,
            (Self::Mapped { with: l, group: gl }, Self::Mapped { with: r, group: gr }) => {
                l.cmp(r).then(gl.cmp(gr))
            }
            (Self::Mapped { with: l, .. }, Self::Explicit(r)) => l.cmp(r).then(Ordering::Less),
            (Self::Explicit(l), Self::Mapped { with: r, .. }) => l.cmp(r).then(Ordering::Greater),
            (Self::Explicit(l), Self::Explicit(r)) => l.cmp(r),
        }
    }
}
impl Capture {
    pub fn name(&self) -> Option<Symbol> {
        self.variable_name().map(|v| v.into_inner())
    }

    pub fn variable_name(&self) -> Option<VariableName> {
        match self {
            Self::Implicit(tv) | Self::Mapped { with: tv, .. } | Self::Explicit(tv) => {
                Some(tv.name)
            }
            Self::Ignore(_) | Self::All(_) => None,
        }
    }

    pub fn group_name(&self) -> Option<Symbol> {
        match self {
            Self::Mapped { group, .. } => Some(*group),
            Self::Explicit(tv) => Some(tv.name.into_inner()),
            Self::Ignore(_) | Self::All(_) | Self::Implicit(_) => None,
        }
    }

    pub fn value_type(&self) -> ValueType {
        match self {
            Self::Implicit(tv) | Self::Mapped { with: tv, .. } | Self::Explicit(tv) => tv.ty,
            Self::All(t) => t.into_inner(),
            Self::Ignore(_) => ValueType::String,
        }
    }
}
impl Spanned for Capture {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Implicit(tv) | Self::Mapped { with: tv, .. } | Self::Explicit(tv) => {
                tv.name.span()
            }
            Self::All(span) => span.span(),
            Self::Ignore(span) => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegexPattern<'a> {
    pub pattern: Span<Cow<'a, str>>,
    pub captures: SmallVec<[Capture; 1]>,
}
impl<'a> RegexPattern<'a> {
    pub fn new(pattern: Span<Cow<'a, str>>) -> Self {
        Self {
            pattern,
            captures: smallvec![],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.pattern.is_empty()
    }

    pub fn len(&self) -> usize {
        self.pattern.len()
    }
}
impl<'a> AsRef<str> for RegexPattern<'a> {
    fn as_ref(&self) -> &str {
        self.pattern.inner().as_ref()
    }
}
impl<'a> Spanned for RegexPattern<'a> {
    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }
}

/// A pattern prefix represents the first distinct
/// subpattern of the overall pattern which can be
/// matched independently of the rest of the pattern.
///
/// Prefixes are used when constructing sets of patterns
/// to find overlapping prefixes that can be collapsed
/// into more efficient searchers for matching.
#[derive(Clone)]
pub enum Prefix<'a> {
    /// The entire pattern is empty
    Empty(SourceSpan),
    /// The entire pattern is a literal string
    Literal(Span<Cow<'a, str>>),
    /// The pattern is a literal string, but only a subset is the prefix
    Substring(Span<Cow<'a, str>>),
    /// The prefix is a simple regular expression
    Regex(RegexPattern<'a>),
    /// The prefix contains a match block/substitution that cannot be
    /// reduced to a regular expression or literal prefix.
    Match(Cow<'a, Match<'a>>),
}
impl<'a> Spanned for Prefix<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Empty(span) => *span,
            Self::Literal(spanned) | Self::Substring(spanned) => spanned.span(),
            Self::Regex(spanned) => spanned.span(),
            Self::Match(spanned) => spanned.span(),
        }
    }
}
impl<'a> Prefix<'a> {
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Empty(_) => Some(""),
            Self::Literal(s) | Self::Substring(s) => Some(s.inner().as_ref()),
            Self::Regex(regex) => Some(regex.pattern.inner().as_ref()),
            Self::Match(_) => None,
        }
    }
}
impl<'a> Eq for Prefix<'a> {}
impl<'a> PartialEq for Prefix<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}
impl<'a> PartialOrd for Prefix<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<'a> Ord for Prefix<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Match(a), Self::Match(b)) => a.cmp(b),
            (Self::Match(_), _) => Ordering::Greater,
            (_, Self::Match(_)) => Ordering::Less,
            (
                Self::Regex(RegexPattern {
                    pattern: ap,
                    captures: ac,
                }),
                Self::Regex(RegexPattern {
                    pattern: bp,
                    captures: bc,
                }),
            ) if !ac.is_empty() && !bc.is_empty() => ap.cmp(bp).then_with(|| ac.cmp(bc)),
            (
                Self::Regex(RegexPattern {
                    pattern: ap,
                    captures: ac,
                }),
                b,
            ) if !ac.is_empty() => ap
                .inner()
                .as_ref()
                .cmp(b.as_str().unwrap())
                .then(Ordering::Greater),
            (
                a,
                Self::Regex(RegexPattern {
                    pattern: bp,
                    captures: bc,
                }),
            ) if !bc.is_empty() => a
                .as_str()
                .unwrap()
                .cmp(bp.inner().as_ref())
                .then(Ordering::Less),
            (a, b) => a.as_str().unwrap().cmp(b.as_str().unwrap()),
        }
    }
}

/// A check pattern is the part of a check line which must match in the check file somewhere
#[derive(Debug)]
pub enum CheckPattern<'a> {
    /// There is no content, we're at the end of line
    Empty(SourceSpan),
    /// The entire pattern is a single raw string
    Literal(Span<Cow<'a, str>>),
    /// The entire pattern is a single regex string
    Regex(RegexPattern<'a>),
    /// The pattern is some mix of literal parts and match rules
    Match(Span<Vec<CheckPatternPart<'a>>>),
}
impl<'a> PartialEq for CheckPattern<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Empty(_), Self::Empty(_)) => true,
            (Self::Literal(l), Self::Literal(r)) => l == r,
            (Self::Regex(l), Self::Regex(r)) => l == r,
            (Self::Match(l), Self::Match(r)) => l == r,
            _ => false,
        }
    }
}
impl<'a> CheckPattern<'a> {
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Empty(_) => true,
            Self::Literal(spanned) => spanned.is_empty(),
            Self::Regex(spanned) => spanned.is_empty(),
            Self::Match(parts) => parts.is_empty(),
        }
    }

    pub fn locate_variables(&self) -> impl Iterator<Item = SourceSpan> + '_ {
        CheckPatternVarIter::Pattern(self)
    }

    pub fn prefix(&self) -> Prefix<'a> {
        match self {
            Self::Literal(literal) => Prefix::Literal(literal.clone()),
            Self::Regex(pattern) => Prefix::Regex(pattern.clone()),
            Self::Match(parts) => match &parts[0] {
                CheckPatternPart::Literal(literal) => Prefix::Substring(literal.clone()),
                CheckPatternPart::Regex(pattern) => Prefix::Regex(pattern.clone()),
                CheckPatternPart::Match(Match::Numeric {
                    span,
                    format,
                    capture: None,
                    expr: None,
                    ..
                }) => Prefix::Regex(RegexPattern::new(Span::new(
                    *span,
                    format.unwrap_or_default().pattern_nocapture(),
                ))),
                CheckPatternPart::Match(Match::Numeric {
                    span,
                    format,
                    capture: Some(name),
                    expr: None,
                    ..
                }) => Prefix::Regex(RegexPattern {
                    pattern: Span::new(*span, format.unwrap_or_default().pattern(None)),
                    captures: smallvec![Capture::Implicit(TypedVariable {
                        name: *name,
                        ty: ValueType::Number(*format),
                    })],
                }),
                CheckPatternPart::Match(Match::Substitution {
                    pattern: Some(pattern),
                    name,
                    ..
                }) => Prefix::Regex(RegexPattern {
                    pattern: pattern.clone(),
                    captures: smallvec![Capture::Implicit(TypedVariable {
                        name: *name,
                        ty: ValueType::String,
                    })],
                }),
                CheckPatternPart::Match(part) => Prefix::Match(Cow::Owned(part.clone())),
            },
            Self::Empty(span) => Prefix::Empty(*span),
        }
    }

    pub fn pop_prefix(&mut self) -> Prefix<'a> {
        use std::collections::VecDeque;

        match self {
            Self::Literal(literal) => {
                let span = literal.span();
                let result = Prefix::Literal(core::mem::replace(
                    literal,
                    Span::new(span, Cow::Borrowed("")),
                ));
                *self = Self::Empty(span);
                result
            }
            Self::Regex(pattern) => {
                let span = pattern.span();
                let result = Prefix::Regex(core::mem::replace(
                    pattern,
                    RegexPattern::new(Span::new(span, Cow::Borrowed(""))),
                ));
                *self = Self::Empty(span);
                result
            }
            Self::Match(parts) => {
                let span = parts.span();
                let mut ps = VecDeque::<CheckPatternPart<'a>>::from(core::mem::take(&mut **parts));
                let prefix = match ps.pop_front().unwrap() {
                    CheckPatternPart::Literal(literal) => Prefix::Substring(literal),
                    CheckPatternPart::Regex(pattern) => Prefix::Regex(pattern),
                    CheckPatternPart::Match(Match::Numeric {
                        span,
                        format,
                        capture: None,
                        expr: None,
                        ..
                    }) => Prefix::Regex(RegexPattern::new(Span::new(
                        span,
                        format.unwrap_or_default().pattern_nocapture(),
                    ))),
                    CheckPatternPart::Match(Match::Numeric {
                        span,
                        format,
                        capture: Some(name),
                        expr: None,
                        ..
                    }) => Prefix::Regex(RegexPattern {
                        pattern: Span::new(span, format.unwrap_or_default().pattern_nocapture()),
                        captures: smallvec![Capture::Implicit(TypedVariable {
                            name,
                            ty: ValueType::Number(format),
                        })],
                    }),
                    CheckPatternPart::Match(Match::Substitution {
                        pattern: Some(pattern),
                        name,
                        ..
                    }) => Prefix::Regex(RegexPattern {
                        pattern,
                        captures: smallvec![Capture::Implicit(TypedVariable {
                            name,
                            ty: ValueType::String,
                        })],
                    }),
                    CheckPatternPart::Match(part) => Prefix::Match(Cow::Owned(part)),
                };
                if ps.is_empty() {
                    *self = Self::Empty(span);
                } else {
                    **parts = ps.into();
                }
                prefix
            }
            Self::Empty(span) => Prefix::Empty(*span),
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            Self::Empty(_) | Self::Literal(_) => true,
            Self::Regex(_) => false,
            Self::Match(parts) => parts
                .iter()
                .all(|p| matches!(p, CheckPatternPart::Literal(_))),
        }
    }

    pub fn is_regex_compatible(&self) -> bool {
        match self {
            Self::Empty(_) | Self::Literal(_) | Self::Regex(_) => true,
            Self::Match(parts) => parts.iter().all(|p| p.is_regex_compatible()),
        }
    }

    /// Compacts this pattern into fewer parts where possible
    pub fn compact(&mut self) {
        use std::collections::VecDeque;

        fn convert_to_regex(buffer: &mut String, padding: usize) {
            let min_capacity = padding
                + buffer
                    .chars()
                    .map(|c| {
                        if regex_syntax::is_meta_character(c) {
                            2
                        } else {
                            1
                        }
                    })
                    .sum::<usize>();
            let prev = core::mem::replace(buffer, String::with_capacity(min_capacity));
            regex_syntax::escape_into(&prev, buffer);
        }

        match self {
            Self::Match(empty) if empty.is_empty() => {
                let span = empty.span();
                *self = Self::Empty(span);
            }
            Self::Match(compacted) => {
                let span = compacted.span();
                let source_id = span.source_id();
                let mut pattern_start = span.start();
                let mut pattern_end = span.end();
                let mut parts = VecDeque::from(core::mem::take(&mut **compacted));
                let mut pattern = String::new();
                let mut captures = SmallVec::<[Capture; 1]>::new();
                let mut is_literal_mode = true;
                while let Some(mut part) = parts.pop_front() {
                    match part {
                        CheckPatternPart::Literal(part) if is_literal_mode => {
                            pattern_end = part.span().end();
                            pattern.push_str(part.inner().as_ref());
                        }
                        CheckPatternPart::Literal(part) => {
                            pattern_end = part.span().end();
                            regex_syntax::escape_into(part.inner().as_ref(), &mut pattern);
                        }
                        CheckPatternPart::Regex(RegexPattern {
                            pattern: part,
                            captures: ref mut part_captures,
                        }) => {
                            let (span, part) = part.into_parts();
                            pattern_end = span.end();
                            captures.append(part_captures);
                            if is_literal_mode {
                                is_literal_mode = false;
                                convert_to_regex(&mut pattern, part.len())
                            }
                            pattern.push_str(part.as_ref());
                        }
                        CheckPatternPart::Match(Match::Substitution {
                            pattern: Some(part),
                            name,
                            span,
                        }) => {
                            pattern_end = span.end();
                            let part = part.into_inner();
                            let group_name = name.as_str();
                            if is_literal_mode {
                                is_literal_mode = false;
                                convert_to_regex(&mut pattern, 6 + group_name.len() + part.len());
                            }
                            pattern.push_str("(?P<");
                            pattern.push_str(group_name);
                            pattern.push('>');
                            pattern.push_str(part.as_ref());
                            pattern.push(')');
                            captures.push(Capture::Explicit(TypedVariable {
                                name,
                                ty: ValueType::String,
                            }));
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            expr: None,
                            capture: None,
                            span,
                            format,
                            ..
                        }) => {
                            pattern_end = span.end();
                            let format_pattern = format.unwrap_or_default().pattern_nocapture();
                            if is_literal_mode {
                                is_literal_mode = false;
                                convert_to_regex(&mut pattern, format_pattern.len());
                            }
                            pattern.push_str(&format_pattern);
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            expr: None,
                            capture: Some(name),
                            span,
                            format,
                            ..
                        }) => {
                            pattern_end = span.end();
                            let group_name = name.as_str();
                            let format_pattern =
                                format.unwrap_or_default().pattern(Some(group_name));
                            if is_literal_mode {
                                is_literal_mode = false;
                                convert_to_regex(&mut pattern, format_pattern.len());
                            }
                            pattern.push_str(&format_pattern);
                            captures.push(Capture::Explicit(TypedVariable {
                                name,
                                ty: ValueType::Number(format),
                            }));
                        }
                        part @ CheckPatternPart::Match(_) => {
                            let span = part.span();
                            if pattern.is_empty() {
                                compacted.push(part);
                                is_literal_mode = true;
                                pattern.clear();
                                captures.clear();
                                pattern_end = span.end();
                                pattern_start = pattern_end;
                                continue;
                            }

                            if is_literal_mode {
                                compacted.push(CheckPatternPart::Literal(Span::new(
                                    SourceSpan::new(
                                        source_id,
                                        Range::new(pattern_start, pattern_end),
                                    ),
                                    Cow::Owned(core::mem::take(&mut pattern)),
                                )));
                            } else {
                                let captures = core::mem::take(&mut captures);
                                compacted.push(CheckPatternPart::Regex(RegexPattern {
                                    pattern: Span::new(
                                        SourceSpan::new(
                                            source_id,
                                            Range::new(pattern_start, pattern_end),
                                        ),
                                        Cow::Owned(core::mem::take(&mut pattern)),
                                    ),
                                    captures,
                                }));
                                is_literal_mode = true;
                            }

                            compacted.push(part);
                            pattern_end = span.end();
                            pattern_start = pattern_end;
                        }
                    }
                }

                if compacted.is_empty() {
                    let compacted = if is_literal_mode {
                        CheckPattern::Literal(Span::new(
                            SourceSpan::new(source_id, Range::new(pattern_start, pattern_end)),
                            Cow::Owned(core::mem::take(&mut pattern)),
                        ))
                    } else {
                        CheckPattern::Regex(RegexPattern {
                            pattern: Span::new(
                                SourceSpan::new(source_id, Range::new(pattern_start, pattern_end)),
                                Cow::Owned(core::mem::take(&mut pattern)),
                            ),
                            captures,
                        })
                    };
                    *self = compacted;
                    return;
                }

                if !pattern.is_empty() {
                    if is_literal_mode {
                        compacted.push(CheckPatternPart::Literal(Span::new(
                            SourceSpan::new(source_id, Range::new(pattern_start, pattern_end)),
                            Cow::Owned(core::mem::take(&mut pattern)),
                        )));
                    } else {
                        compacted.push(CheckPatternPart::Regex(RegexPattern {
                            pattern: Span::new(
                                SourceSpan::new(source_id, Range::new(pattern_start, pattern_end)),
                                Cow::Owned(core::mem::take(&mut pattern)),
                            ),
                            captures,
                        }));
                    }
                }
            }
            Self::Empty(_) | Self::Literal(_) | Self::Regex(_) => (),
        }
    }

    /// Converts this pattern into a string which can be used as a
    /// regular expression, even if the pattern was not originally
    /// expressed as a regular expression.
    ///
    /// Returns Err with the original pattern (potentially compacted),
    /// if the conversion is not possible. Otherwise, returns Ok
    /// with the built regular expression pattern.
    pub fn into_regex_pattern(mut self) -> Result<RegexPattern<'a>, Self> {
        self.compact();

        match self {
            Self::Literal(s) => Ok(RegexPattern::new(s)),
            Self::Regex(regex) => Ok(regex),
            other => Err(other),
        }
    }
}
impl<'a> Spanned for CheckPattern<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Empty(span) => *span,
            Self::Literal(spanned) => spanned.span(),
            Self::Regex(spanned) => spanned.span(),
            Self::Match(spanned) => spanned.span(),
        }
    }
}
impl<'a> From<Vec<CheckPatternPart<'a>>> for CheckPattern<'a> {
    fn from(mut parts: Vec<CheckPatternPart<'a>>) -> Self {
        match parts.len() {
            0 => CheckPattern::Empty(SourceSpan::UNKNOWN),
            1 => match parts.pop().unwrap() {
                CheckPatternPart::Literal(lit) => Self::Literal(lit),
                CheckPatternPart::Regex(re) => Self::Regex(re),
                part @ CheckPatternPart::Match(_) => {
                    Self::Match(Span::new(part.span(), vec![part]))
                }
            },
            _ => {
                let start = parts.first().unwrap().span().start();
                let last_span = parts.last().unwrap().span();
                let end = last_span.end();
                Self::Match(Span::new(
                    SourceSpan::new(last_span.source_id(), Range::new(start, end)),
                    parts,
                ))
            }
        }
    }
}

/// A check line is broken up into segments when either `[[` `]]`,
/// or `{{` `}}` is encountered, for substitutions/captures and regex
/// matches respectively; with the before and after parts being literal
/// (and optional). As such we have three types of segments/parts that
/// we can observe on a line
#[derive(Debug, PartialEq, Eq)]
pub enum CheckPatternPart<'a> {
    /// This part consists of a match rule to be evaluated while matching
    Match(Match<'a>),
    /// This part is a raw literal string
    Literal(Span<Cow<'a, str>>),
    /// This part is a regex pattern
    Regex(RegexPattern<'a>),
}
impl<'a> CheckPatternPart<'a> {
    pub fn unwrap_str(self) -> Span<Cow<'a, str>> {
        match self {
            Self::Literal(s) => s,
            part => panic!("expected a literal pattern, got {part:#?}"),
        }
    }

    pub fn uses_variable(&self) -> bool {
        match self {
            Self::Literal(_) | Self::Regex(_) => false,
            Self::Match(Match::Numeric {
                expr: None,
                capture: None,
                ..
            }) => false,
            Self::Match(_) => true,
        }
    }

    pub fn is_regex_compatible(&self) -> bool {
        match self {
            Self::Literal(_)
            | Self::Regex(_)
            | Self::Match(Match::Substitution {
                pattern: Some(_), ..
            })
            | Self::Match(Match::Numeric { expr: None, .. }) => true,
            Self::Match(_) => false,
        }
    }
}
impl<'a> Spanned for CheckPatternPart<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Match(m) => m.span(),
            Self::Literal(spanned) => spanned.span(),
            Self::Regex(spanned) => spanned.span(),
        }
    }
}

/// This type represents a match rule wrapped in `[[` `]]`
#[derive(Debug, Clone)]
pub enum Match<'a> {
    /// Match the given regular expression pattern, optionally binding `name`
    /// to the matched value.
    ///
    /// Corresponds to expressions such as `[[REG]]` and `[[REG:r[0-9]+]]`.
    ///
    /// The precise format of this match type is `[[<name>:<pattern>]]`, where:
    ///
    /// * `<name>` is a local variable name of the form `[A-Za-z_][A-Za-z0-9_]*`, or a global
    ///   variable name (prefixed with `$`). However, you are not permitted to (re)bind global
    ///   variables.
    /// * `:<pattern>`, is any valid, non-empty, regular expression pattern. When present, it
    ///   changes the semantics of this match type from string substitution to string capture - i.e.
    ///   `name` will be bound to the matched input string.
    ///
    /// If `:<pattern>` is not present, then the entire `[[<name>]]` block will be
    /// substituted with the value of `<name>` as a literal pattern. The value will
    /// be formatted according to its type.
    ///
    /// Variables bound using this syntax are available immediately on the same line, you
    /// can do things like `CHECK: op [[REG:r[0-9]+]], [[REG]]` to bind `REG` to the register
    /// name of the first operand of `op`, e.g., `r1`; and verify that the same register is
    /// used as the second operand.
    ///
    /// NOTE: You should prefer the standard regular expression pattern matching syntax,
    /// i.e. `{{<pattern>}}` if you don't need to bind a variable.
    Substitution {
        span: SourceSpan,
        name: VariableName,
        pattern: Option<Span<Cow<'a, str>>>,
    },
    /// Match the given numeric pattern, and optionally defines a variable if the
    /// match succeeds.
    ///
    /// Corresponds to expressions such as `[[#]]` or `[[#%.8X]]` or `[[#REG + 1]]`,
    /// as well as `[[#REG:]]` and `[[#%X,OFFSET:12]]`. The former are matches,
    /// while the latter both match and define the given variable.
    ///
    /// The unified format is `[[#%<fmtspec>,<NUMVAR>: <constraint> <expr]]` where:
    ///
    /// * `%<fmtspec>` is the same format specifier as used for defining a variable, but in this
    ///   context it indicates how the numeric value should be matched. It is optional, and if not
    ///   present, both components of the format spec are inferred from the matching format of the
    ///   numeric variables used by the expression constraint (if any), and defaults to `%u`
    ///   (unsigned, no leading zeros) if no numeric variable is used. In case of conflict between
    ///   format specifiers of several numeric variables, the conversion specifier becomes
    ///   mandatory, but the precision specifier remains optional.
    /// * `<NUMVAR>:`, when present, indicates that `NUMVAR` will be (re)bound to the matched value,
    ///   if the match succeeds. If not present, no variable is defined.
    /// * `<constraint>` describes how the value to match must relate to the value of the given
    ///   expression. Currently, the only constraint type is `==` for equality. If present, `<expr>`
    ///   is mandatory; however the inverse is not true, `<expr>` can be provided without
    ///   `<constraint>`, implying a default equality constraint.
    /// * `<expr>` is an expression. An expression is in turn recursively defined as:
    ///   - A numeric operand
    ///   - An expression followed by an operator and a numeric operand
    ///
    ///   A numeric operand is a previously defined numeric variable, an integer literal,
    ///   or one of a set of built-in functions. Whitespace are allowed around these elements.
    ///   Numeric operands are 64-bit values. Overflow and underflow are rejected. The original
    ///   `lit` does not support operator precedence, but `litcheck` supports the standard precedence
    ///   of the supported operators, and parentheses can be used to manually manage precedence.
    ///
    ///   The operators supported are:
    ///
    ///   - `+`, addition
    ///   - `-`, subtraction
    ///
    ///   The built-in functions supported are:
    ///
    ///   - `add`, addition
    ///   - `sub`, subtraction
    ///   - `mul`, multiplication
    ///   - `div`, integer division
    ///   - `min`, minimum
    ///   - `max`, maximum
    ///
    /// All components can be omitted except the `#`, i.e. `[[#]]` is a valid numeric match,
    /// which defaults to matching an unsigned integer, with no leading zeros, of up to 64
    /// bit precision.
    Numeric {
        span: SourceSpan,
        /// The format of the value to match.
        ///
        /// If not specified, it is implied by the format
        /// of any numeric operands in `expr`, otherwise it
        /// defaults to an unsigned integer with no leading zeros.
        format: Option<NumberFormat>,
        /// If set, contains the name of the variable to bind to
        /// the matched value if the match succeeds.
        capture: Option<VariableName>,
        /// If specified, this changes the meaning of `expr`
        /// in relation to the matched value.
        constraint: Constraint,
        /// The numeric expression to evaluate
        ///
        /// If `constraint` is not set, this expression
        /// produces a value which must match the input.
        expr: Option<Expr>,
    },
}
impl<'a> PartialOrd for Match<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<'a> Ord for Match<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (
                Self::Substitution {
                    name: an,
                    pattern: Some(ap),
                    ..
                },
                Self::Substitution {
                    name: bn,
                    pattern: Some(bp),
                    ..
                },
            ) => ap.cmp(bp).then_with(|| an.cmp(bn)),
            (
                Self::Substitution {
                    pattern: Some(_), ..
                },
                Self::Substitution { pattern: None, .. },
            ) => Ordering::Less,
            (
                Self::Substitution { pattern: None, .. },
                Self::Substitution {
                    pattern: Some(_), ..
                },
            ) => Ordering::Greater,
            (Self::Substitution { name: an, .. }, Self::Substitution { name: bn, .. }) => {
                an.cmp(bn)
            }
            (
                Self::Numeric {
                    format: af,
                    capture: None,
                    expr: aexpr,
                    ..
                },
                Self::Numeric {
                    format: bf,
                    capture: None,
                    expr: bexpr,
                    ..
                },
            ) => af
                .unwrap_or_default()
                .pattern(None)
                .cmp(&bf.unwrap_or_default().pattern(None))
                .then_with(|| aexpr.cmp(bexpr)),
            (
                Self::Numeric { capture: None, .. },
                Self::Numeric {
                    capture: Some(_), ..
                },
            ) => Ordering::Less,
            (
                Self::Numeric {
                    capture: Some(_), ..
                },
                Self::Numeric { capture: None, .. },
            ) => Ordering::Greater,
            (
                Self::Numeric {
                    format: af,
                    capture: Some(acap),
                    expr: aexpr,
                    ..
                },
                Self::Numeric {
                    format: bf,
                    capture: Some(bcap),
                    expr: bexpr,
                    ..
                },
            ) => af
                .unwrap_or_default()
                .pattern(None)
                .cmp(&bf.unwrap_or_default().pattern(None))
                .then_with(|| acap.cmp(bcap))
                .then_with(|| aexpr.cmp(bexpr)),
            (
                Self::Substitution {
                    name,
                    pattern: Some(pattern),
                    ..
                },
                Self::Numeric {
                    format,
                    capture,
                    expr: None,
                    ..
                },
            ) => AsRef::<str>::as_ref(pattern)
                .cmp(format.unwrap_or_default().pattern(None).as_ref())
                .then_with(|| Some(*name).cmp(capture))
                .then(Ordering::Less),
            (
                Self::Numeric {
                    format,
                    capture,
                    expr: None,
                    ..
                },
                Self::Substitution {
                    name,
                    pattern: Some(pattern),
                    ..
                },
            ) => format
                .unwrap_or_default()
                .pattern(None)
                .as_ref()
                .cmp(pattern.inner().as_ref())
                .then_with(|| capture.cmp(&Some(*name)))
                .then(Ordering::Greater),
            (Self::Substitution { .. }, _) => Ordering::Less,
            (_, Self::Substitution { .. }) => Ordering::Greater,
        }
    }
}
impl<'a> Spanned for Match<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Numeric { span, .. } | Self::Substitution { span, .. } => *span,
        }
    }
}
impl<'a> Eq for Match<'a> {}
impl<'a> PartialEq for Match<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Substitution {
                    name: an,
                    pattern: ap,
                    ..
                },
                Self::Substitution {
                    name: bn,
                    pattern: bp,
                    ..
                },
            ) => an == bn && ap == bp,
            (
                Self::Numeric {
                    format: af,
                    capture: acap,
                    constraint: ac,
                    expr: aexpr,
                    ..
                },
                Self::Numeric {
                    format: bf,
                    capture: bcap,
                    constraint: bc,
                    expr: bexpr,
                    ..
                },
            ) => af == bf && acap == bcap && ac == bc && aexpr == bexpr,
            _ => false,
        }
    }
}

/// Describes available constraints that can be expressed on numeric values
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Constraint {
    Eq,
}

#[derive(Default)]
enum CheckPatternVarIter<'a, 'iter> {
    #[default]
    Empty,
    Pattern(&'iter CheckPattern<'a>),
    Regex(&'iter [Capture]),
    Parts(&'iter [CheckPatternPart<'a>]),
    Expr {
        expr: &'iter Expr,
        parts: &'iter [CheckPatternPart<'a>],
    },
    Buffered {
        buffer: std::collections::VecDeque<SourceSpan>,
        next: &'iter [CheckPatternPart<'a>],
    },
}
impl<'a, 'iter> Iterator for CheckPatternVarIter<'a, 'iter> {
    type Item = SourceSpan;

    fn next(&mut self) -> Option<Self::Item> {
        'outer: loop {
            match core::mem::take(self) {
                Self::Empty => break None,
                Self::Pattern(pattern) => match pattern {
                    CheckPattern::Empty(_) | CheckPattern::Literal(_) => break None,
                    CheckPattern::Regex(re) => {
                        let (item, rest) = re.captures.split_first()?;
                        *self = Self::Regex(rest);
                        break Some(item.span());
                    }
                    CheckPattern::Match(parts) => {
                        *self = Self::Parts(parts);
                        continue;
                    }
                },
                Self::Regex(captures) => {
                    let (item, rest) = captures.split_first()?;
                    *self = Self::Regex(rest);
                    break Some(item.span());
                }
                Self::Parts(parts) => {
                    while let Some((part, parts)) = parts.split_first() {
                        match part {
                            CheckPatternPart::Literal(_) => break,
                            CheckPatternPart::Regex(re) => match re.captures.split_first() {
                                Some((item, vars)) => {
                                    *self = Self::Buffered {
                                        buffer: vars.iter().map(|v| v.span()).collect(),
                                        next: parts,
                                    };
                                    break 'outer Some(item.span());
                                }
                                None => break,
                            },
                            CheckPatternPart::Match(Match::Substitution { name, .. }) => {
                                *self = Self::Parts(parts);
                                break 'outer Some(name.span());
                            }
                            CheckPatternPart::Match(Match::Numeric {
                                capture: None,
                                expr: None,
                                ..
                            }) => {
                                continue;
                            }
                            CheckPatternPart::Match(Match::Numeric {
                                capture,
                                expr: Some(expr),
                                ..
                            }) => {
                                *self = Self::Expr { expr, parts };
                                if let Some(name) = capture.as_ref() {
                                    break 'outer Some(name.span());
                                }
                                continue 'outer;
                            }
                            CheckPatternPart::Match(Match::Numeric {
                                capture: Some(name),
                                expr: None,
                                ..
                            }) => {
                                *self = Self::Parts(parts);
                                break 'outer Some(name.span());
                            }
                        }
                    }

                    break None;
                }
                Self::Expr { expr, parts } => {
                    let mut worklist = std::collections::VecDeque::with_capacity(2);
                    let mut buffer = std::collections::VecDeque::new();
                    worklist.push_back(expr);
                    loop {
                        let expr = worklist.pop_front();
                        match expr {
                            None => match buffer.pop_front() {
                                None => {
                                    *self = Self::Parts(parts);
                                    continue 'outer;
                                }
                                Some(span) => {
                                    *self = Self::Buffered {
                                        buffer,
                                        next: parts,
                                    };
                                    break 'outer Some(span);
                                }
                            },
                            Some(Expr::Num(_)) => {
                                continue;
                            }
                            Some(Expr::Var(name)) => {
                                buffer.push_back(name.span());
                            }
                            Some(Expr::Binary { lhs, rhs, .. }) => {
                                worklist.push_back(lhs);
                                worklist.push_back(rhs);
                            }
                        }
                    }
                }
                Self::Buffered { mut buffer, next } => match buffer.pop_front() {
                    None => {
                        *self = Self::Parts(next);
                    }
                    Some(span) => {
                        *self = Self::Buffered { buffer, next };
                        break Some(span);
                    }
                },
            }
        }
    }
}
