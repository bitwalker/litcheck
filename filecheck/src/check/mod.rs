pub mod bytecode;
mod checker;
mod input;
pub mod matchers;
mod rules;
mod test;

use self::bytecode::{CheckProgram, Pattern, PatternSet};
pub use self::checker::{Checker, TestFailed};
pub use self::input::Input;
use self::matchers::*;
use self::rules::Rule;
pub use self::test::FileCheckTest;

use std::{borrow::Cow, fmt, str::FromStr};

use litcheck::diagnostics::{DiagResult, Diagnostic, Report, SourceSpan, Span, Spanned};

use crate::{expr::*, Config};

pub const DEFAULT_CHECK_PREFIXES: &[&str] = &["CHECK"];
pub const DEFAULT_COMMENT_PREFIXES: &[&str] = &["COM", "RUN"];

#[derive(Diagnostic, Debug, thiserror::Error)]
pub enum CheckError {
    #[error("check failed: invalid expression in directive: {0}")]
    #[diagnostic(transparent)]
    Expr(#[from] ExprError),
}

/// A check file is the source file we're going to check matches some set of rules.
///
/// The rules to check are found by parsing the file into lines, and recognizing
/// certain special patterns that indicate what checks are expected to be performed.
///
/// Once checks are parsed from a file, the file is checked line-by-line, using the
/// parsed checks to determine if the file passes or not.
#[derive(Debug)]
pub struct CheckFile<'a> {
    /// There should be an entry in this vector for every line in the source file
    lines: Vec<CheckLine<'a>>,
}
impl<'a> CheckFile<'a> {
    pub fn new(lines: Vec<CheckLine<'a>>) -> Self {
        Self { lines }
    }

    pub fn lines(&self) -> &[CheckLine<'a>] {
        self.lines.as_slice()
    }

    pub fn into_lines(self) -> Vec<CheckLine<'a>> {
        self.lines
    }

    pub fn compile(self, config: &'a Config) -> DiagResult<CheckProgram<'a>> {
        CheckProgram::compile(self, config)
    }
}

/// A check line represents a line in a check file, and is associated with some
/// check type and pattern.
#[derive(Debug)]
pub struct CheckLine<'a> {
    /// Where in the source file that the check was specified
    pub span: SourceSpan,
    /// Any comment lines preceding this check in the sourrce file
    pub comment: Vec<Cow<'a, str>>,
    /// The type of check represented
    pub ty: CheckType,
    /// The pattern to match
    pub pattern: CheckPattern<'a>,
}
impl<'a> CheckLine<'a> {
    pub fn new(span: SourceSpan, ty: CheckType, pattern: CheckPattern<'a>) -> Self {
        Self {
            span,
            comment: vec![],
            ty,
            pattern,
        }
    }

    #[inline(always)]
    pub fn kind(&self) -> Check {
        self.ty.kind
    }

    pub fn has_variable(&self) -> Option<&CheckPatternPart<'a>> {
        self.pattern.has_variable()
    }

    pub fn with_comment(mut self, comment: Cow<'a, str>) -> Self {
        if comment.is_empty() {
            return self;
        }
        self.comment.push(comment);
        self
    }

    pub fn into_comment(mut self) -> Cow<'a, str> {
        match self.comment.len() {
            0 => Cow::Borrowed(""),
            1 => self.comment.pop().unwrap(),
            n => {
                let len = self.comment.iter().map(|c| c.len()).sum::<usize>() + n;
                Cow::Owned(self.comment.into_iter().fold(
                    String::with_capacity(len),
                    |mut buf, c| {
                        if !buf.is_empty() {
                            buf.push('\n');
                        }
                        buf.push_str(&c);
                        buf
                    },
                ))
            }
        }
    }

    pub fn prepend_comment(&mut self, comment: Cow<'a, str>) {
        if comment.is_empty() {
            return;
        }
        self.comment.insert(0, comment);
    }
}
impl<'a> Spanned for CheckLine<'a> {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl<'a> Eq for CheckLine<'a> {}
impl<'a> PartialEq for CheckLine<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty && self.pattern == other.pattern && self.comment == other.comment
    }
}

#[derive(Debug)]
pub struct CheckType {
    span: SourceSpan,
    pub kind: Check,
    pub modifiers: Span<CheckModifier>,
}
impl Default for CheckType {
    fn default() -> Self {
        Self::new(SourceSpan::from(0..0), Default::default())
    }
}
impl Spanned for CheckType {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl CheckType {
    pub fn new(span: SourceSpan, kind: Check) -> Self {
        Self {
            span,
            kind,
            modifiers: Span::new(span, Default::default()),
        }
    }

    pub fn with_modifiers(mut self, modifiers: Span<CheckModifier>) -> Self {
        self.modifiers = modifiers;
        self
    }

    pub fn is_literal_match(&self) -> bool {
        self.modifiers.contains(CheckModifier::LITERAL)
    }

    pub fn count(&self) -> usize {
        self.modifiers.count()
    }
}
impl Eq for CheckType {}
impl PartialEq for CheckType {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.modifiers == other.modifiers
    }
}

#[derive(Debug)]
pub enum InvalidCheckTypeError {
    Unrecognized,
    InvalidCount(core::num::ParseIntError),
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub enum Check {
    #[default]
    None,
    Plain,
    Next,
    Same,
    Not,
    Dag,
    Label,
    Empty,
    Count(usize),
    Comment,
}
impl Check {
    pub fn suffix(&self) -> Option<&'static str> {
        match self {
            Self::Plain => Some(""),
            Self::Next => Some("-NEXT"),
            Self::Same => Some("-SAME"),
            Self::Not => Some("-NOT"),
            Self::Dag => Some("-DAG"),
            Self::Label => Some("-LABEL"),
            Self::Empty => Some("-EMPTY"),
            Self::Count(_) => Some("-COUNT"),
            Self::Comment | Self::None => None,
        }
    }
}
impl fmt::Display for Check {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::None => f.write_str("CHECK-NONE"),
            Self::Plain => f.write_str("CHECK"),
            Self::Next => f.write_str("CHECK-NEXT"),
            Self::Same => f.write_str("CHECK-SAME"),
            Self::Not => f.write_str("CHECK-NOT"),
            Self::Dag => f.write_str("CHECK-DAG"),
            Self::Label => f.write_str("CHECK-LABEL"),
            Self::Empty => f.write_str("CHECK-EMPTY"),
            Self::Count(n) => write!(f, "CHECK-COUNT-{n}"),
            Self::Comment => f.write_str("COM"),
        }
    }
}
impl FromStr for Check {
    type Err = InvalidCheckTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "" => Ok(Self::Plain),
            "NEXT" | "next" => Ok(Self::Next),
            "SAME" | "same" => Ok(Self::Same),
            "NOT" | "not" => Ok(Self::Not),
            "DAG" | "dag" => Ok(Self::Dag),
            "LABEL" | "label" => Ok(Self::Label),
            "EMPTY" | "empty" => Ok(Self::Empty),
            _ => match s
                .strip_prefix("COUNT-")
                .or_else(|| s.strip_prefix("count-"))
            {
                None => Err(InvalidCheckTypeError::Unrecognized),
                Some(count) => count
                    .parse::<usize>()
                    .map_err(InvalidCheckTypeError::InvalidCount)
                    .map(Self::Count),
            },
        }
    }
}

#[derive(Debug)]
pub struct InvalidCheckModifierError;

bitflags::bitflags! {
    #[derive(Copy, Clone)]
    pub struct CheckModifier: u16 {
        const LITERAL = 1;
        const COUNT = 2;
    }
}
impl Default for CheckModifier {
    fn default() -> Self {
        Self::empty()
    }
}
impl FromStr for CheckModifier {
    type Err = InvalidCheckModifierError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "LITERAL" | "literal" => Ok(Self::LITERAL),
            _ => Err(InvalidCheckModifierError),
        }
    }
}
impl fmt::Debug for CheckModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("CheckModifier(")?;
        let mut mods = 0;
        if self.contains(Self::LITERAL) {
            mods += 1;
            f.write_str("LITERAL")?;
        }
        if self.contains(Self::COUNT) {
            if mods > 0 {
                f.write_str(" | ")?;
            }
            write!(f, "COUNT({})", self.count())?;
        }
        Ok(())
    }
}
impl CheckModifier {
    pub fn is_literal(&self) -> bool {
        self.contains(Self::LITERAL)
    }

    pub fn from_count(count: u8) -> Self {
        let count = (count as u16) << 2;
        Self::COUNT | CheckModifier::from_bits_retain(count)
    }

    pub fn count(&self) -> usize {
        if self.contains(Self::COUNT) {
            (self.bits() >> 2) as usize
        } else {
            1
        }
    }
}
impl Eq for CheckModifier {}
impl PartialEq for CheckModifier {
    fn eq(&self, other: &Self) -> bool {
        self.count() == other.count() && self.is_literal() == other.is_literal()
    }
}

/// A check pattern is the part of a check line which must match in the check file somewhere
#[derive(Debug, PartialEq)]
pub enum CheckPattern<'a> {
    /// There is no content, we're at the end of line
    Empty,
    /// The entire pattern is a single raw string
    Literal(Span<&'a str>),
    /// The entire pattern is a single regex string
    Regex(Span<&'a str>),
    /// The pattern is some mix of literal parts and match rules
    Match(Span<Vec<CheckPatternPart<'a>>>),
}
impl<'a> CheckPattern<'a> {
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Empty => true,
            Self::Literal(ref spanned) => spanned.is_empty(),
            Self::Regex(_) | Self::Match(_) => false,
        }
    }

    pub fn has_variable(&self) -> Option<&CheckPatternPart<'a>> {
        match self {
            Self::Empty | Self::Literal(_) | Self::Regex(_) => None,
            Self::Match(ref parts) => parts.iter().find(|p| p.has_variable()),
        }
    }
}
impl<'a> Spanned for CheckPattern<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Empty => SourceSpan::from(0..0),
            Self::Literal(ref spanned) => spanned.span(),
            Self::Regex(ref spanned) => spanned.span(),
            Self::Match(ref spanned) => spanned.span(),
        }
    }
}
impl<'a> From<Vec<CheckPatternPart<'a>>> for CheckPattern<'a> {
    fn from(mut parts: Vec<CheckPatternPart<'a>>) -> Self {
        match parts.len() {
            0 => CheckPattern::Empty,
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

/// This type wraps related diagnostics for use with [CheckFailedError]
#[derive(Debug)]
pub struct RelatedError(Report);
impl RelatedError {
    pub fn into_report(self) -> Report {
        self.0
    }

    #[inline(always)]
    pub fn as_diagnostic(&self) -> &dyn Diagnostic {
        self.0.as_ref()
    }
}
impl Diagnostic for RelatedError {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.as_diagnostic().code()
    }
    fn severity(&self) -> Option<litcheck::diagnostics::Severity> {
        self.as_diagnostic().severity()
    }
    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.as_diagnostic().help()
    }
    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.as_diagnostic().url()
    }
    fn source_code(&self) -> Option<&dyn litcheck::diagnostics::SourceCode> {
        self.as_diagnostic().source_code()
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = litcheck::diagnostics::LabeledSpan> + '_>> {
        self.as_diagnostic().labels()
    }
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.as_diagnostic().related()
    }
    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.as_diagnostic().diagnostic_source()
    }
}
impl fmt::Display for RelatedError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}
impl std::error::Error for RelatedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        AsRef::<dyn std::error::Error>::as_ref(&self.0).source()
    }
}
impl From<Report> for RelatedError {
    fn from(report: Report) -> Self {
        Self(report)
    }
}
impl RelatedError {
    pub const fn new(report: Report) -> Self {
        Self(report)
    }

    pub fn wrap<E>(error: E) -> Self
    where
        E: Diagnostic + Send + Sync + 'static,
    {
        Self(Report::new_boxed(Box::new(error)))
    }
}

#[derive(Diagnostic, Debug, thiserror::Error)]
pub enum CheckFailedError {
    /// Indicates a match for an excluded pattern.
    #[error("match found, but was excluded")]
    #[diagnostic()]
    MatchFoundButExcluded {
        #[label("match found here")]
        span: SourceSpan,
        #[source_code]
        input_file: litcheck::diagnostics::ArcSource,
        #[related]
        pattern: Option<RelatedCheckError>,
    },
    /// Indicates a match for an expected pattern, but the match is on the
    /// wrong line.
    #[error("match found for expected pattern, but on the wrong line")]
    #[diagnostic()]
    MatchFoundButWrongLine {
        #[label("match found here")]
        span: SourceSpan,
        #[source_code]
        input_file: litcheck::diagnostics::ArcSource,
        #[related]
        pattern: Option<RelatedCheckError>,
    },
    /// Indicates a discarded match for an expected pattern.
    #[error("match found, but was discarded")]
    #[diagnostic()]
    MatchFoundButDiscarded {
        #[label("match found here")]
        span: SourceSpan,
        #[source_code]
        input_file: litcheck::diagnostics::ArcSource,
        #[related]
        pattern: Option<RelatedCheckError>,
        #[help]
        note: Option<String>,
    },
    /// Indicates an error while processing a match after the match was found
    /// for an expected or excluded pattern.  The error is specified by `Note`,
    /// to which it should be appropriate to prepend "error: " later.  The full
    /// match itself should be recorded in a preceding diagnostic of a different
    /// `MatchFound*` match type.
    #[error("match found, but there was an error processing it")]
    #[diagnostic()]
    MatchFoundErrorNote {
        #[label("match found here")]
        span: SourceSpan,
        #[source_code]
        input_file: litcheck::diagnostics::ArcSource,
        #[related]
        error: Option<RelatedError>,
    },
    /// Indicates no match for an expected pattern, but this might follow good
    /// matches when multiple matches are expected for the pattern, or it might
    /// follow discarded matches for the pattern.
    #[error("no matches were found for expected pattern")]
    #[diagnostic()]
    MatchNoneButExpected {
        #[label("pattern at this location was not matched")]
        span: SourceSpan,
        #[source_code]
        match_file: litcheck::diagnostics::ArcSource,
        #[help]
        note: Option<String>,
    },
    /// Indicates no match due to an expected or excluded pattern that has
    /// proven to be invalid at match time.  The exact problems are usually
    /// reported in subsequent diagnostics of the same match type but with
    /// `Note` set.
    #[error("unable to match invalid pattern")]
    #[diagnostic()]
    MatchNoneForInvalidPattern {
        #[label("pattern at this location was invalid")]
        span: SourceSpan,
        #[source_code]
        match_file: litcheck::diagnostics::ArcSource,
        #[related]
        error: Option<RelatedError>,
    },
    /// Indicates a fuzzy match that serves as a suggestion for the next
    /// intended match for an expected pattern with too few or no good matches.
    #[error("an exact match was not found, but some similar matches were found, see notes")]
    #[diagnostic()]
    MatchFuzzy {
        #[label("pattern at this location was invalid")]
        span: SourceSpan,
        #[source_code]
        match_file: litcheck::diagnostics::ArcSource,
        #[help]
        notes: Option<String>,
    },
}

/// This is used to associated source spans from the match file
/// with those from the input file.
#[derive(Diagnostic, Debug, thiserror::Error)]
#[error("check failed")]
#[diagnostic()]
pub struct RelatedCheckError {
    #[label("due to pattern at this location")]
    pub span: SourceSpan,
    #[source_code]
    pub match_file: litcheck::diagnostics::ArcSource,
}

#[derive(Debug)]
pub enum MatchType {
    /// Indicates a good match for an expected pattern.
    MatchFoundAndExpected,
    /// Indicates no match for an excluded pattern.
    MatchNoneAndExcluded,
    /// The match failed for some reason
    Failed(CheckFailedError),
}
impl MatchType {
    pub fn is_ok(&self) -> bool {
        matches!(
            self,
            Self::MatchFoundAndExpected | Self::MatchNoneAndExcluded
        )
    }
}
impl fmt::Display for MatchType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::MatchFoundAndExpected => f.write_str("match found for expected pattern"),
            Self::MatchNoneAndExcluded => f.write_str("excluded pattern was never matched"),
            Self::Failed(err) => write!(f, "{err}"),
        }
    }
}
