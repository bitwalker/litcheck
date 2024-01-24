#![allow(unused, dead_code)]
use std::{borrow::Cow, fmt};

use litcheck::{
    diagnostics::{Diag, DiagResult, SourceSpan, Span, Spanned},
    range::Range,
    Symbol,
};
use regex_automata::meta::Regex;

use crate::{
    check::{Constraint, Input},
    expr::{Expr, NumberFormat, Value, ValueType, VariableName},
};

use super::{regex::RegexMatcher, substring::SubstringMatcher, *};

/// This matcher is used to match a single pattern that is a
/// hybrid of various other matchers, e.g. a literal string,
/// followed by a numeric capture expression, followed by a
/// regular expression, followed by a substitution of the
/// previously-captured numeric value.
///
/// Because some of these components are context-sensitive, or
/// cannot be resolved until runtime, we cannot compile a single
/// regular expression to match them. Instead we compile the
/// various parts we do know ahead-of-time into optimized searchers
/// and leave only the "holes" to be resolved and dynamically compiled
/// at runtime.
pub struct HybridMatcher<'a> {
    /// The span of the pattern in the check file from
    /// which this matcher was derived
    span: SourceSpan,
    /// The compiled form of the input regex, with holes
    /// in the regex ready to be populated at runtime when
    /// executing a search on a given line
    pattern: LazyRegex<'a>,
    /// Metadata about captures in the pattern
    captures: Vec<ValueType>,
}
impl<'a> fmt::Debug for HybridMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("HybridMatcher")
            .field("pattern", &self.pattern)
            .finish()
    }
}
impl<'a> HybridMatcher<'a> {
    pub fn new(span: SourceSpan) -> Self {
        Self {
            span,
            pattern: Default::default(),
            captures: vec![],
        }
    }

    #[inline(always)]
    pub fn builder<'short, 'long: 'short>(&'long mut self) -> &'short mut LazyRegex<'a> {
        &mut self.pattern
    }
}
impl<'a> Matcher for HybridMatcher<'a> {
    fn try_match<'input>(&self, _input: Input<'input>) -> Option<MatchInfo<'input>> {
        todo!()
    }
}
impl<'a> Spanned for HybridMatcher<'a> {
    fn span(&self) -> SourceSpan {
        self.span
    }
}

/// A regex-like matcher that contains "holes" representing bits of
/// data that must be evaluated at runtime in the context of a specific
/// portion of the input buffer.
///
/// In general, the intention is for this to behave like a regular
/// expression from an external perspective, but internally it is a
/// blend of multiple matcher components to handle each of the "hole"
/// types that are supported
#[derive(Debug, Default)]
pub struct LazyRegex<'a> {
    /// The parts of the regex
    ///
    /// These parts are matched and evaluated left-to-right,
    /// each one is fully processed before moving to the next
    /// part. The regular expression has its own workspace for
    /// locally-bound variables and the like, so that if the
    /// pattern fails, none of the state changes are applied
    /// globally
    parts: Vec<LazyRegexPart<'a>>,
}
impl<'a> LazyRegex<'a> {
    pub fn literal(&mut self, part: Span<Cow<'a, str>>) -> &mut Self {
        self.parts.push(LazyRegexPart::Literal(part));
        self
    }

    pub fn regex(&mut self, part: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        let (span, pattern) = part.into_parts();
        let pattern = todo!();
        self.parts.push(LazyRegexPart::Regex {
            span,
            pattern,
            capture: false,
        });
        Ok(self)
    }

    pub fn numeric(&mut self, span: SourceSpan, format: NumberFormat) -> &mut Self {
        self.parts.push(LazyRegexPart::Numeric { span, format });
        self.parts.push(LazyRegexPart::Drop);
        self
    }

    pub fn numeric_with_constraint(
        &mut self,
        span: SourceSpan,
        format: NumberFormat,
        constraint: Constraint,
        expr: Expr,
    ) -> &mut Self {
        self.parts.push(LazyRegexPart::Numeric { span, format });
        self.parts.push(LazyRegexPart::Constraint {
            span,
            constraint,
            expr,
        });
        self.parts.push(LazyRegexPart::Drop);
        self
    }

    pub fn substitution(&mut self, expr: Expr) -> &mut Self {
        self.parts
            .push(LazyRegexPart::Substitution { expr, ty: None });
        self
    }

    pub fn substitution_with_format(&mut self, expr: Expr, ty: ValueType) -> &mut Self {
        self.parts
            .push(LazyRegexPart::Substitution { expr, ty: Some(ty) });
        self
    }

    pub fn capture(&mut self, name: Symbol, pattern: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        let (span, pattern) = pattern.into_parts();
        let pattern = todo!();
        self.parts.push(LazyRegexPart::Regex {
            span,
            pattern,
            capture: true,
        });
        self.parts.push(LazyRegexPart::Bind { name, ty: None });
        Ok(self)
    }

    pub fn capture_numeric(
        &mut self,
        span: SourceSpan,
        name: Symbol,
        format: NumberFormat,
    ) -> &mut Self {
        self.parts.push(LazyRegexPart::Numeric { span, format });
        self.parts.push(LazyRegexPart::Bind {
            name,
            ty: Some(ValueType::Number(format)),
        });
        self
    }

    pub fn capture_numeric_with_constraint(
        &mut self,
        span: SourceSpan,
        name: Symbol,
        format: NumberFormat,
        constraint: Constraint,
        expr: Expr,
    ) -> &mut Self {
        let span = expr.span();
        self.parts.push(LazyRegexPart::Numeric { span, format });
        self.parts.push(LazyRegexPart::Constraint {
            span,
            constraint,
            expr,
        });
        self.parts.push(LazyRegexPart::Bind {
            name,
            ty: Some(ValueType::Number(format)),
        });
        self
    }
}

/// A part of a [LazyRegex] to be evaluated on-demand.
///
/// Each part of a [LazyRegex] is anchored at the current position
/// in the input when it is matched, and the entire regex fails if
/// the search does not match at the anchor.
enum LazyRegexPart<'a> {
    /// A literal string to match
    Literal(Span<Cow<'a, str>>),
    /// A compiled regular expression to match
    Regex {
        /// The span of the pattern
        span: SourceSpan,
        /// The compiled regular expression
        pattern: Regex,
        /// If true, pushes the matched input on the operand stack
        capture: bool,
    },
    /// Match a numeric value of the given format, and push it on the operand stack.
    Numeric {
        /// The span of the pattern
        span: SourceSpan,
        /// The format of the number to match
        format: NumberFormat,
    },
    /// Pop a value off the operand stack and bind it to `name`
    Bind {
        name: Symbol,
        /// If set, the value on the operand stack will be converted
        /// to a value of the given type, otherwise the match fails.
        ty: Option<ValueType>,
    },
    /// Drop the top value on the operand stack
    Drop,
    /// Evaluate a constraint at the current execution point against
    /// the value on top of the operand stack.
    Constraint {
        /// The span of the pattern which is being constrained
        span: SourceSpan,
        /// The constraint type
        constraint: Constraint,
        /// The constraint
        expr: Expr,
    },
    /// Substitute a value into the pattern by evaluating an expression and formatting it
    Substitution {
        /// The expression whose value once evaluated will be substituted for this part
        expr: Expr,
        /// The type to format the value of `expr` as, if specified
        ///
        /// Defaults to the format of the value returned by `expr`
        ty: Option<ValueType>,
    },
}
impl<'a> fmt::Debug for LazyRegexPart<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Literal(lit) => f.debug_tuple("Literal").field(lit).finish(),
            Self::Regex {
                pattern, capture, ..
            } => f
                .debug_struct("Regex")
                .field("pattern", pattern)
                .field("capture", capture)
                .finish(),
            Self::Numeric { format, .. } => f.debug_tuple("Numeric").field(format).finish(),
            Self::Bind { name, ty, .. } => f
                .debug_struct("Bind")
                .field("name", name)
                .field("ty", ty)
                .finish(),
            Self::Drop => f.write_str("Drop"),
            Self::Constraint {
                constraint, expr, ..
            } => f
                .debug_struct("Constraint")
                .field("constraint", constraint)
                .field("expr", expr)
                .finish(),
            Self::Substitution { expr, ty, .. } => f
                .debug_struct("Substitution")
                .field("expr", expr)
                .field("ty", ty)
                .finish(),
        }
    }
}
