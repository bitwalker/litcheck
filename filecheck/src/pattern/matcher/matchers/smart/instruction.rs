use regex_automata::PatternID;

use crate::pattern::matcher::SubstringMatcher;
use crate::{ast::Capture, common::*, expr::ValueType};

/// A single instruction for the [SmartMatcher] evaluation loop.
///
/// Each op which performs a match is anchored at the current position
/// in the input, except for the first one, which will search for the
/// first occurrance of that pattern anywhere in the input.
///
/// If an op fails, the entire match attempt fails.
pub enum MatchOp<'a> {
    /// Match a literal string
    Literal(SubstringMatcher<'a>),
    /// Match a regular expression, optionally capturing a string value.
    Regex {
        /// The original pattern as a string
        source: Span<Cow<'a, str>>,
        /// The compiled regular expression
        pattern: Regex,
        /// If set, this is the capture group data corresponding to
        /// the `captures` set in [SmartMatcher]. This will be used to
        /// collect the captured value and push it on the operand
        /// stack.
        captures: SmallVec<[CaptureGroup; 1]>,
    },
    /// Match a numeric value of the given format
    Numeric {
        /// The span of the pattern
        span: SourceSpan,
        /// The format of the number to match
        format: Option<NumberFormat>,
        /// If set, this is the capture group data corresponding to
        /// the `captures` set in [SmartMatcher]. This will be used to
        /// collect the captured value and push it on the operand
        /// stack.
        capture: Option<CaptureGroup>,
    },
    /// Match a regular expression that is constructed dynamically
    /// by substituting a value produced by evaluating the given
    /// expression, and then formatting it as specified.
    Substitution {
        /// The expression whose value once evaluated will be substituted for this part
        expr: Expr,
        /// The type to format the value of `expr` as, if specified
        ///
        /// Defaults to the format of the value returned by `expr`
        ty: Option<ValueType>,
    },
    /// Pop a value off the operand stack and bind it to `name`
    Bind {
        /// The name to bind
        name: VariableName,
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
}
impl<'a> fmt::Debug for MatchOp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Literal(lit) => f.debug_tuple("Literal").field(lit).finish(),
            Self::Regex {
                source,
                pattern,
                captures,
                ..
            } => {
                let config = pattern.get_config();
                let group_info = pattern.group_info();
                f.debug_struct("Regex")
                    .field("pattern", source)
                    .field("pattern_len", &pattern.pattern_len())
                    .field(
                        "groups",
                        &group_info
                            .all_names()
                            .collect::<smallvec::SmallVec<[_; 4]>>(),
                    )
                    .field("match_kind", &config.get_match_kind())
                    .field("captures", captures)
                    .field("memory_usage", &pattern.memory_usage())
                    .finish()
            }
            Self::Numeric {
                format, capture, ..
            } => f
                .debug_struct("Numeric")
                .field("format", format)
                .field("capture", capture)
                .finish(),
            Self::Bind { name, ty, .. } => f
                .debug_struct("Bind")
                .field("name", &format!("{name}"))
                .field("ty", ty)
                .finish(),
            Self::Drop => f.write_str("Drop"),
            Self::Constraint {
                constraint, expr, ..
            } => f
                .debug_struct("Constraint")
                .field("constraint", constraint)
                .field("expr", &format!("{expr}"))
                .finish(),
            Self::Substitution { expr, ty, .. } => f
                .debug_struct("Substitution")
                .field("expr", &format!("{expr}"))
                .field("ty", ty)
                .finish(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct CaptureGroup {
    pub pattern_id: PatternID,
    pub group_id: usize,
    pub info: Capture,
}
