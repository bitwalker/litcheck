use litcheck::diagnostics::{DiagResult, Report, Spanned};

use crate::{check::*, expr::*, Config};

/// The compiled form of [CheckPattern]
#[derive(Debug)]
pub enum Pattern<'a> {
    /// A literal string that must occur somewhere in the input
    Substring(SubstringMatcher<'a>),
    /// A regular expression that must occur somewhere in the input
    Regex(RegexMatcher),
    /// A hybrid match expression that must occur somewhere in the input
    Hybrid(HybridMatcher<'a>),
    /// A matcher for pure ASCII whitespace patterns
    Whitespace(AsciiWhitespaceMatcher),
}
impl<'a> Matcher for Pattern<'a> {
    fn try_match<'input>(&self, input: Input<'input>) -> Option<MatchInfo<'input>> {
        match self {
            Self::Substring(ref matcher) => matcher.try_match(input),
            Self::Regex(ref matcher) => matcher.try_match(input),
            Self::Hybrid(ref matcher) => matcher.try_match(input),
            Self::Whitespace(ref matcher) => matcher.try_match(input),
        }
    }
}
impl<'a> Spanned for Pattern<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Substring(ref matcher) => matcher.span(),
            Self::Regex(ref matcher) => matcher.span(),
            Self::Hybrid(ref matcher) => matcher.span(),
            Self::Whitespace(ref matcher) => matcher.span(),
        }
    }
}

#[derive(Debug)]
pub enum PatternSet<'a> {
    /// Match a set of literal strings
    Literal(MultiSubstringMatcher<'a>),
    /// Match a set of static regex patterns
    Regex(RegexSetMatcher<'a>),
    // Match a set of regex patterns, including both static and dynamic patterns
    //Tree(PatternTree<'a>),
}
impl<'a> Spanned for PatternSet<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Literal(ref matcher) => matcher.span(),
            Self::Regex(ref matcher) => matcher.span(),
        }
    }
}

/// Used during construction of a [PatternTree] to represent a [PatternSet]
pub enum DelayedPatternSet<'a> {
    Literal(Vec<Span<Cow<'a, str>>>),
    Regex(Vec<CheckPattern<'a>>),
    Match(Vec<Vec<CheckPatternPart<'a>>>),
    Mixed(Vec<CheckPattern<'a>>),
}
impl<'a> DelayedPatternSet<'a> {
    pub fn into_pattern_set(self) -> DiagResult<PatternSet<'a>> {
        todo!()
    }
}

/// 1. Issue instructions to search for each CHECK-LABEL: directive (if present)
/// and divide the input into blocks.
/// 2. Move to the start of the first block
/// 3. For each check directive, seek to the pattern, the test implicitly fails
/// if the seek never matches. Unless the subsequent check is CHECK-SAME, we insert
/// an Advance instruction after the seek, to ensure we move to the next line.
/// 4. If there are multiple blocks, insert a SwitchToBlock at the end of each block
/// that moves to the next block of the input.
#[derive(Debug)]
pub enum CheckOp<'a> {
    /// Like IndexOf, but also marks the start of a new block scope
    ///
    /// When evaluated, the checker will see if there are remaining blocks
    /// in the program, and if so, find the index of the next BlockStart
    BlockStart(Pattern<'a>),
    ApplyRule(Box<dyn Rule + 'a>),
    ApplyRulePrecededBy(Box<dyn Rule + 'a>, Box<dyn Rule + 'a>),
    RepeatRule(Box<dyn Rule + 'a>, usize),
    RepeatRulePrecededBy(Box<dyn Rule + 'a>, usize, Box<dyn Rule + 'a>),
}

#[derive(Diagnostic, Debug, thiserror::Error)]
pub enum InvalidCheckFileError {
    #[error("check file did not contain any rules")]
    #[diagnostic()]
    Empty,
    #[error("invalid CHECK-LABEL pattern")]
    #[diagnostic()]
    CheckLabelVariable {
        #[label("in this pattern")]
        line: SourceSpan,
        #[label("variables/substitutions are not allowed on CHECK-LABEL lines")]
        var: SourceSpan,
    },
    #[error("{kind} directives are not permitted to be the first directive in a file")]
    #[diagnostic()]
    InvalidFirstCheck {
        #[label]
        line: SourceSpan,
        kind: Check,
    },
    #[error("invalid CHECK pattern")]
    #[diagnostic()]
    EmptyPattern(#[label("expected a non-empty pattern here")] SourceSpan),
}

#[derive(Default)]
pub struct CheckProgram<'a> {
    pub code: Vec<CheckOp<'a>>,
}
impl<'a> CheckProgram<'a> {
    pub fn compile(check_file: CheckFile<'a>, _config: &'a Config) -> DiagResult<Self> {
        // 1. Identify and split lines into blocks
        // 2. For each block, compile the rules of that block
        let mut program = Self::default();

        let mut lines = check_file.into_lines();
        if lines.is_empty() {
            return Err(Report::from(InvalidCheckFileError::Empty));
        }

        let mut blocks = vec![];
        let mut num_lines = lines.len();
        let mut ln = 0;
        'outer: while ln < num_lines {
            if lines[ln].kind() == Check::Label {
                // Split the current block at `ln` if we aren't at the beginning
                if ln > 0 {
                    let mut split = lines.split_off(ln + 1);
                    core::mem::swap(&mut lines, &mut split);
                    blocks.push(split);
                    ln = 0;
                    num_lines = lines.len();
                }
                // Search until we find the next check-label, or end of file
                let mut block_ln = ln + 1;
                while block_ln < num_lines {
                    if lines[block_ln].kind() == Check::Label {
                        let mut split = lines.split_off(block_ln);
                        core::mem::swap(&mut lines, &mut split);
                        blocks.push(split);
                        ln = 0;
                        num_lines = lines.len();
                        continue 'outer;
                    }
                    block_ln += 1;
                }
                // We reached end of file, so all blocks have been seen;
                break;
            }
            // Skip over other directives
            ln += 1;
        }

        // If we have not recorded any blocks, then the entire file
        // is in a single logical block. Similarly, if there are lines
        // that we have not yet added to a block, append them as a
        // new block.
        if blocks.is_empty() {
            assert!(!lines.is_empty());
            blocks.push(lines);
        } else if !lines.is_empty() {
            blocks.push(lines);
        }

        // Now, all blocks have been computed, so process each block
        for block in blocks.into_iter().filter(|b| !b.is_empty()) {
            program.compile_block(block)?;
        }

        Ok(program)
    }

    fn compile_block(&mut self, block: Vec<CheckLine<'a>>) -> DiagResult<()> {
        let mut lines = block.into_iter().peekable();

        // Handle labeled block prologue
        let is_labeled = lines
            .peek()
            .map(|line| matches!(line.kind(), Check::Label))
            .unwrap_or(false);
        if is_labeled {
            let line = lines.next().unwrap();
            assert_eq!(line.kind(), Check::Label);
            if let Some(var) = line.has_variable() {
                return Err(Report::from(InvalidCheckFileError::CheckLabelVariable {
                    line: line.span(),
                    var: var.span(),
                }));
            }
            let pattern = self.compile_pattern(line.pattern)?;
            self.push(CheckOp::BlockStart(pattern));
        }

        // Emit the body of the block
        let mut before: Option<Box<dyn Rule + 'a>> = None;
        while let Some(line) = lines.next() {
            match line.kind() {
                Check::Dag => {
                    let mut dags = vec![line];
                    loop {
                        if matches!(lines.peek().map(|l| l.kind()), Some(Check::Dag)) {
                            dags.push(lines.next().unwrap());
                            continue;
                        }
                        break;
                    }
                    let rule = self.compile_rule_from_many(Check::Dag, dags)?;
                    if let Some(before) = before.take() {
                        self.push(CheckOp::ApplyRulePrecededBy(rule, before));
                    } else {
                        self.push(CheckOp::ApplyRule(rule));
                    }
                }
                Check::Not => {
                    let mut nots = vec![line];
                    loop {
                        if matches!(lines.peek().map(|l| l.kind()), Some(Check::Not)) {
                            nots.push(lines.next().unwrap());
                            continue;
                        }
                        break;
                    }
                    let rule = self.compile_rule_from_many(Check::Not, nots)?;
                    assert!(before.replace(rule).is_none());
                }
                Check::Empty => {
                    if self.code.is_empty() {
                        return Err(Report::from(InvalidCheckFileError::InvalidFirstCheck {
                            kind: Check::Empty,
                            line: line.span(),
                        }));
                    }
                    let rule = Box::new(rules::CheckEmpty::new(line.span()));
                    if let Some(before) = before.take() {
                        self.push(CheckOp::ApplyRulePrecededBy(rule, before));
                    } else {
                        self.push(CheckOp::ApplyRule(rule));
                    }
                }
                kind @ (Check::Plain | Check::Same | Check::Next) => {
                    if kind != Check::Plain && self.code.is_empty() {
                        return Err(Report::from(InvalidCheckFileError::InvalidFirstCheck {
                            kind,
                            line: line.span(),
                        }));
                    }
                    let rule = self.compile_rule(line.ty, line.pattern)?;
                    if let Some(before) = before.take() {
                        self.push(CheckOp::ApplyRulePrecededBy(rule, before));
                    } else {
                        self.push(CheckOp::ApplyRule(rule));
                    }
                }
                Check::Count(n) => {
                    let rule = self.compile_rule(line.ty, line.pattern)?;
                    if let Some(before) = before.take() {
                        self.push(CheckOp::RepeatRulePrecededBy(rule, n, before));
                    } else {
                        self.push(CheckOp::RepeatRule(rule, n));
                    }
                }
                Check::None | Check::Comment => continue,
                Check::Label => {
                    unreachable!("all check-label directives should have been stripped")
                }
            }
        }

        // If there are remaining unordered CHECK-(DAG|NOT) lines, emit
        // an implicit check for those items to the end of the block/file
        if let Some(before) = before.take() {
            self.push(CheckOp::ApplyRule(before));
        }

        Ok(())
    }

    fn compile_rule(
        &mut self,
        ty: CheckType,
        pattern: CheckPattern<'a>,
    ) -> DiagResult<Box<dyn Rule + 'a>> {
        let pattern = if ty.is_literal_match() {
            self.compile_literal_pattern(pattern)?
        } else {
            self.compile_pattern(pattern)?
        };

        match ty.kind {
            Check::Plain | Check::Count(_) => Ok(Box::new(rules::CheckPlain::new(pattern))),
            Check::Same => Ok(Box::new(rules::CheckSame::new(pattern))),
            Check::Next => Ok(Box::new(rules::CheckNext::new(pattern))),
            kind => unreachable!("we should never be compiling a rule for {kind} here"),
        }
    }

    fn compile_rule_from_many(
        &mut self,
        kind: Check,
        lines: Vec<CheckLine<'a>>,
    ) -> DiagResult<Box<dyn Rule + 'a>> {
        let set = self.compile_pattern_set(lines)?;
        match kind {
            Check::Dag => Ok(Box::new(rules::CheckDag::new(set))),
            Check::Not => Ok(Box::new(rules::CheckNot::new(set))),
            _ => panic!("{kind} is not valid in this context"),
        }
    }

    fn compile_pattern_set(&mut self, unordered: Vec<CheckLine<'a>>) -> DiagResult<PatternSet<'a>> {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        enum PatternSetType {
            Unknown,
            Literal,
            Regex,
            Match,
            Mixed,
        }

        let set_type =
            unordered
                .iter()
                .fold(PatternSetType::Unknown, |acc, line| match line.pattern {
                    CheckPattern::Literal(_) => match acc {
                        PatternSetType::Literal | PatternSetType::Regex | PatternSetType::Mixed => {
                            acc
                        }
                        PatternSetType::Match => PatternSetType::Mixed,
                        PatternSetType::Unknown => PatternSetType::Literal,
                    },
                    CheckPattern::Regex(_) => match acc {
                        PatternSetType::Regex | PatternSetType::Mixed => acc,
                        PatternSetType::Match => PatternSetType::Mixed,
                        PatternSetType::Literal | PatternSetType::Unknown => PatternSetType::Regex,
                    },
                    CheckPattern::Match(_) => match acc {
                        PatternSetType::Match | PatternSetType::Mixed => acc,
                        PatternSetType::Regex | PatternSetType::Literal => PatternSetType::Mixed,
                        PatternSetType::Unknown => PatternSetType::Match,
                    },
                    CheckPattern::Empty => {
                        unreachable!("this pattern should never be part of a match set")
                    }
                });

        let delayed = match set_type {
            PatternSetType::Literal => DelayedPatternSet::Literal(
                unordered
                    .into_iter()
                    .map(|line| match line.pattern {
                        CheckPattern::Literal(s) => s.map(Cow::Borrowed),
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>(),
            ),
            PatternSetType::Regex => DelayedPatternSet::Regex(
                unordered
                    .into_iter()
                    .map(|line| match line.pattern {
                        p @ (CheckPattern::Literal(_) | CheckPattern::Regex(_)) => p,
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>(),
            ),
            PatternSetType::Match => DelayedPatternSet::Match(
                unordered
                    .into_iter()
                    .map(|line| match line.pattern {
                        CheckPattern::Match(m) => m.into_inner(),
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>(),
            ),
            PatternSetType::Mixed => DelayedPatternSet::Match(
                unordered
                    .into_iter()
                    .filter_map(|line| match line.pattern {
                        CheckPattern::Literal(s) => Some(vec![CheckPatternPart::Literal(s)]),
                        CheckPattern::Regex(s) => Some(vec![CheckPatternPart::Regex(s)]),
                        CheckPattern::Match(m) => Some(m.into_inner()),
                        CheckPattern::Empty => None,
                    })
                    .collect::<Vec<_>>(),
            ),
            PatternSetType::Unknown => panic!("invalid empty pattern set"),
        };

        delayed.into_pattern_set()
    }

    fn compile_pattern(&mut self, pattern: CheckPattern<'a>) -> DiagResult<Pattern<'a>> {
        match pattern {
            CheckPattern::Literal(s) => {
                if s.is_empty() {
                    return Err(Report::from(InvalidCheckFileError::EmptyPattern(s.span())));
                }
                if s.trim().is_empty() {
                    if s.chars().all(|c| c.is_ascii_whitespace()) {
                        Ok(Pattern::Whitespace(AsciiWhitespaceMatcher::new(s.span())))
                    } else {
                        Ok(Pattern::Regex(RegexMatcher::new(Span::new(
                            s.span(),
                            Cow::Borrowed(r"\s+"),
                        ))?))
                    }
                } else {
                    Ok(Pattern::Substring(SubstringMatcher::new(
                        s.map(Cow::Borrowed),
                    )?))
                }
            }
            CheckPattern::Regex(s) => Ok(Pattern::Regex(RegexMatcher::new(s.map(Cow::Borrowed))?)),
            CheckPattern::Match(s) => {
                let (span, parts) = s.into_parts();
                let mut matcher = HybridMatcher::new(span);
                let builder = matcher.builder();
                for part in parts.into_iter() {
                    match part {
                        CheckPatternPart::Literal(s) => {
                            builder.literal(s.map(Cow::Borrowed));
                        }
                        CheckPatternPart::Regex(s) => {
                            builder.regex(s.map(Cow::Borrowed))?;
                        }
                        CheckPatternPart::Match(Match::Substitution {
                            name,
                            pattern: None,
                            ..
                        }) => {
                            builder.substitution(Expr::Var(name));
                        }
                        CheckPatternPart::Match(Match::Substitution {
                            span,
                            name,
                            pattern: Some(pattern),
                        }) => {
                            builder.capture(
                                name.into_inner(),
                                Span::new(span, Cow::Borrowed(pattern.into_inner())),
                            )?;
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            span,
                            format,
                            capture: None,
                            expr: None,
                            ..
                        }) => {
                            builder.numeric(span, format);
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            format,
                            capture: None,
                            expr: Some(expr),
                            ..
                        }) => {
                            builder.substitution_with_format(expr, ValueType::Number(format));
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            span,
                            format,
                            capture: Some(name),
                            expr: None,
                            ..
                        }) => {
                            builder.capture_numeric(span, name.into_inner(), format);
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            span,
                            format,
                            capture: Some(name),
                            constraint,
                            expr: Some(expr),
                        }) => {
                            builder.capture_numeric_with_constraint(
                                span,
                                name.into_inner(),
                                format,
                                constraint,
                                expr,
                            );
                        }
                    }
                }

                Ok(Pattern::Hybrid(matcher))
            }
            CheckPattern::Empty => unreachable!(
                "{pattern:?} is only valid for CHECK-EMPTY, and is not an actual pattern"
            ),
        }
    }

    fn compile_literal_pattern(&mut self, pattern: CheckPattern<'a>) -> DiagResult<Pattern<'a>> {
        match pattern {
            CheckPattern::Literal(lit) => Ok(Pattern::Substring(SubstringMatcher::new(
                lit.map(Cow::Borrowed),
            )?)),
            CheckPattern::Regex(_) | CheckPattern::Match(_) => {
                unreachable!("the lexer will never emit tokens for these non-terminals")
            }
            CheckPattern::Empty => unreachable!(
                "{pattern:?} is only valid for CHECK-EMPTY, and is not an actual pattern"
            ),
        }
    }

    #[inline(always)]
    fn push(&mut self, op: CheckOp<'a>) {
        self.code.push(op);
    }
}
