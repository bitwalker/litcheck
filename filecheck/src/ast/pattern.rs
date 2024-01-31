use crate::common::*;

/// A check pattern is the part of a check line which must match in the check file somewhere
#[derive(Debug)]
pub enum CheckPattern<'a> {
    /// There is no content, we're at the end of line
    Empty(SourceSpan),
    /// The entire pattern is a single raw string
    Literal(Span<&'a str>),
    /// The entire pattern is a single regex string
    Regex(Span<&'a str>),
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
            Self::Literal(ref spanned) => spanned.is_empty(),
            Self::Regex(_) | Self::Match(_) => false,
        }
    }

    pub fn prefix(&self) -> Option<(Span<Cow<'a, str>>, bool)> {
        match self {
            Self::Literal(literal) => Some((literal.map(Cow::Borrowed), true)),
            Self::Regex(pattern) => Some((pattern.map(Cow::Borrowed), false)),
            Self::Match(parts) => match &parts[0] {
                CheckPatternPart::Literal(literal) => Some((literal.map(Cow::Borrowed), true)),
                CheckPatternPart::Regex(pattern) => Some((pattern.map(Cow::Borrowed), false)),
                CheckPatternPart::Match(Match::Numeric {
                    span,
                    format,
                    capture: None,
                    expr: None,
                    ..
                }) => Some((Span::new(*span, Cow::Owned(format.pattern())), false)),
                CheckPatternPart::Match(_) => None,
            },
            Self::Empty(_) => None,
        }
    }

    pub fn pop_prefix(&mut self) -> Option<(Span<Cow<'a, str>>, bool)> {
        use std::collections::VecDeque;

        match self {
            Self::Literal(literal) => {
                let span = literal.span();
                let result = Some((literal.map(Cow::Borrowed), true));
                *self = Self::Empty(span);
                result
            }
            Self::Regex(pattern) => {
                let span = pattern.span();
                let result = Some((pattern.map(Cow::Borrowed), false));
                *self = Self::Empty(span);
                result
            }
            Self::Match(ref mut parts) => {
                let span = parts.span();
                let mut ps = VecDeque::<CheckPatternPart<'a>>::from(core::mem::take(&mut **parts));
                let prefix = match ps.pop_front().unwrap() {
                    CheckPatternPart::Literal(literal) => Some((literal.map(Cow::Borrowed), true)),
                    CheckPatternPart::Regex(pattern) => Some((pattern.map(Cow::Borrowed), false)),
                    CheckPatternPart::Match(Match::Numeric {
                        span,
                        format,
                        capture: None,
                        expr: None,
                        ..
                    }) => Some((Span::new(span, Cow::Owned(format.pattern())), false)),
                    part @ CheckPatternPart::Match(_) => {
                        ps.push_front(part);
                        None
                    }
                };
                if prefix.is_some() {
                    if ps.is_empty() {
                        *self = Self::Empty(span);
                    } else {
                        *parts = Span::new(span, ps.into());
                    }
                }
                prefix
            }
            Self::Empty(_) => None,
        }
    }

    pub fn has_variable(&self) -> Option<&CheckPatternPart<'a>> {
        match self {
            Self::Empty(_) | Self::Literal(_) | Self::Regex(_) => None,
            Self::Match(ref parts) => parts.iter().find(|p| p.has_variable()),
        }
    }
}
impl<'a> Spanned for CheckPattern<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Empty(span) => *span,
            Self::Literal(ref spanned) => spanned.span(),
            Self::Regex(ref spanned) => spanned.span(),
            Self::Match(ref spanned) => spanned.span(),
        }
    }
}
impl<'a> From<Vec<CheckPatternPart<'a>>> for CheckPattern<'a> {
    fn from(mut parts: Vec<CheckPatternPart<'a>>) -> Self {
        match parts.len() {
            0 => CheckPattern::Empty(SourceSpan::from(0..0)),
            1 => match parts.pop().unwrap() {
                CheckPatternPart::Literal(lit) => Self::Literal(lit),
                CheckPatternPart::Regex(re) => Self::Regex(re),
                part @ CheckPatternPart::Match(_) => {
                    Self::Match(Span::new(part.span(), vec![part]))
                }
            },
            _ => {
                let start = parts.first().unwrap().span().offset();
                let last_span = parts.last().unwrap().span();
                let end = last_span.offset() + last_span.len();
                Self::Match(Span::new(SourceSpan::from(start..end), parts))
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
    Literal(Span<&'a str>),
    /// This part is a regex pattern
    Regex(Span<&'a str>),
}
impl<'a> CheckPatternPart<'a> {
    pub fn has_variable(&self) -> bool {
        match self {
            Self::Literal(_) | Self::Regex(_) => false,
            Self::Match(_) => true,
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
#[derive(Debug)]
pub enum Match<'a> {
    /// Match the given regular expression pattern, optionally binding `name`
    /// to the matched value.
    ///
    /// Corresponds to expressions such as `[[REG]]` and `[[REG:r[0-9]+]]`.
    ///
    /// The precise format of this match type is `[[<name>:<pattern>]]`, where:
    ///
    /// * `<name>` is a local variable name of the form `[A-Za-z_][A-Za-z0-9_]*`,
    /// or a global variable name (prefixed with `$`). However, you are not permitted
    /// to (re)bind global variables.
    ///
    /// * `:<pattern>`, is any valid, non-empty, regular expression pattern. When present,
    /// it changes the semantics of this match type from string substitution to string
    /// capture - i.e. `name` will be bound to the matched input string.
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
        pattern: Option<Span<&'a str>>,
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
    /// * `%<fmtspec>` is the same format specifier as used for defining a variable, but
    /// in this context it indicates how the numeric value should be matched. It is optional,
    /// and if not present, both components of the format spec are inferred from the matching
    /// format of the numeric variables used by the expression constraint (if any), and
    /// defaults to `%u` (unsigned, no leading zeros) if no numeric variable is used. In
    /// case of conflict between format specifiers of several numeric variables, the
    /// conversion specifier becomes mandatory, but the precision specifier remains optional.
    ///
    /// * `<NUMVAR>:`, when present, indicates that `NUMVAR` will be (re)bound to the matched
    /// value, if the match succeeds. If not present, no variable is defined.
    ///
    /// * `<constraint>` describes how the value to match must relate to the value of the
    /// given expression. Currently, the only constraint type is `==` for equality. If present,
    /// `<expr>` is mandatory; however the inverse is not true, `<expr>` can be provided
    /// without `<constraint>`, implying a default equality constraint.
    ///
    /// * `<expr>` is an expression. An expression is in turn recursively defined as:
    ///
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
        format: NumberFormat,
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
