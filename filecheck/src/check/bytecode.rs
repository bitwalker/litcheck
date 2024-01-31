use litcheck::diagnostics::{DiagResult, Report, Spanned};

use crate::{check::*, Config};

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
    BlockStart(SimpleMatcher<'a>),
    ApplyRule(Box<dyn DynRule + 'a>),
    ApplyRulePrecededBy(Box<dyn DynRule + 'a>, Box<dyn DynRule + 'a>),
    RepeatRule(Box<dyn DynRule + 'a>, usize),
    RepeatRulePrecededBy(Box<dyn DynRule + 'a>, usize, Box<dyn DynRule + 'a>),
}

#[derive(Default)]
pub struct CheckProgram<'a> {
    pub code: Vec<CheckOp<'a>>,
}
impl<'a> CheckProgram<'a> {
    pub fn compile(
        check_file: CheckFile<'a>,
        _config: &Config,
        interner: &mut StringInterner,
    ) -> DiagResult<Self> {
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
            program.compile_block(block, interner)?;
        }

        Ok(program)
    }

    fn compile_block(
        &mut self,
        block: Vec<CheckLine<'a>>,
        interner: &mut StringInterner,
    ) -> DiagResult<()> {
        let mut lines = block.into_iter().peekable();

        // Handle labeled block prologue
        let is_labeled = lines
            .peek()
            .map(|line| matches!(line.kind(), Check::Label))
            .unwrap_or(false);
        if is_labeled {
            let line = lines.next().unwrap();
            assert_eq!(line.kind(), Check::Label);

            let pattern = Pattern::compile_static(line.span, line.pattern)?;
            self.push(CheckOp::BlockStart(pattern));
        }

        // Emit the body of the block
        let mut before: Option<Box<dyn DynRule + 'a>> = None;
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
                    let rule = self.compile_rule_from_many(Check::Dag, dags, interner)?;
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
                    let rule = self.compile_rule_from_many(Check::Not, nots, interner)?;
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
                    let rule = self.compile_rule(line.ty, line.pattern, interner)?;
                    if let Some(before) = before.take() {
                        self.push(CheckOp::ApplyRulePrecededBy(rule, before));
                    } else {
                        self.push(CheckOp::ApplyRule(rule));
                    }
                }
                Check::Count(n) => {
                    let rule = self.compile_rule(line.ty, line.pattern, interner)?;
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
        interner: &mut StringInterner,
    ) -> DiagResult<Box<dyn DynRule + 'a>> {
        let pattern = if ty.is_literal_match() {
            Pattern::compile_literal(pattern)?
        } else {
            Pattern::compile(pattern, interner)?
        };

        match ty.kind {
            Check::Plain | Check::Count(_) => {
                Ok(Box::new(rules::CheckPlain::new(pattern.into_matcher_mut())))
            }
            Check::Same => Ok(Box::new(rules::CheckSame::new(pattern.into_matcher_mut()))),
            Check::Next => Ok(Box::new(rules::CheckNext::new(pattern.into_matcher_mut()))),
            kind => unreachable!("we should never be compiling a rule for {kind} here"),
        }
    }

    fn compile_rule_from_many(
        &mut self,
        kind: Check,
        lines: Vec<CheckLine<'a>>,
        interner: &mut StringInterner,
    ) -> DiagResult<Box<dyn DynRule + 'a>> {
        let match_all = MatchAll::compile(lines, interner)?;
        match kind {
            Check::Dag => Ok(Box::new(rules::CheckDag::new(match_all))),
            Check::Not => Ok(Box::new(rules::CheckNot::new(match_all))),
            _ => panic!("{kind} is not valid in this context"),
        }
    }

    #[inline(always)]
    fn push(&mut self, op: CheckOp<'a>) {
        self.code.push(op);
    }
}
