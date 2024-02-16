use std::collections::VecDeque;

use crate::{
    ast::*,
    common::*,
    errors::InvalidCheckFileError,
    pattern::matcher::{MatchAll, MatchAny, SimpleMatcher},
    rules::*,
};

/// A tree of patterns to match on the same region of input.
///
/// Once all patterns are matched, the matches are checked
/// to ensure that they appear in the correct order.
#[derive(Debug)]
pub enum CheckTree<'a> {
    /// The leaf node of this tree is a set of patterns to match as a group
    Leaf(CheckGroup<'a>),
    /// A two-way branch of the tree, rooted at a CHECK-NOT directive
    Both {
        /// Non-leaf nodes of the tree are rooted at a CHECK-NOT directive
        root: MatchAny<'a>,
        /// The patterns which must match before `root`
        left: Box<CheckTree<'a>>,
        /// The patterns which must match after `root`
        right: Box<CheckTree<'a>>,
    },
    /// A single-branch node, rooted at a CHECK-NOT directive
    Left {
        /// Non-leaf nodes of the tree are rooted at a CHECK-NOT directive
        root: MatchAny<'a>,
        /// The patterns which must match before `root`
        left: Box<CheckTree<'a>>,
    },
    /// A single-branch node, rooted at a CHECK-NOT directive
    Right {
        /// Non-leaf nodes of the tree are rooted at a CHECK-NOT directive
        root: MatchAny<'a>,
        /// The patterns which must match after `root`
        right: Box<CheckTree<'a>>,
    },
}
impl<'a> CheckTree<'a> {
    pub fn leftmost(&self) -> Either<&CheckGroup<'a>, &MatchAny<'a>> {
        match self {
            Self::Leaf(ref group) => Left(group),
            Self::Both { ref left, .. } | Self::Left { ref left, .. } => left.leftmost(),
            Self::Right { ref root, .. } => Right(root),
        }
    }

    pub fn rightmost(&self) -> Either<&CheckGroup<'a>, &MatchAny<'a>> {
        match self {
            Self::Leaf(ref group) => Left(group),
            Self::Both { ref right, .. } | Self::Right { ref right, .. } => right.rightmost(),
            Self::Left { ref root, .. } => Right(root),
        }
    }
}

#[derive(Debug)]
pub enum CheckGroup<'a> {
    /// This group type occurs when there are no patterns
    /// except for a CHECK-NOT in a logical group. This is
    /// a special case, but is preferable to representing
    /// this using [CheckTree]
    Never(MatchAny<'a>),
    /// A group of rules that can be matched in any order,
    /// but must not overlap each other, and must not extend
    /// past any matches in subsequent groups. This latter
    /// property is verified lazily, after the true bounds
    /// of the group are known.
    Unordered(Box<CheckDag<'a>>),
    /// This is a special group type, used to anchor the search for
    /// a CHECK-DAG rule to a following CHECK. This is purely used to
    /// improve the accuracy of diagnostics related to match errors
    /// involving this specific rule transition.
    Bounded {
        /// The CHECK-DAG rule to match
        left: Box<CheckDag<'a>>,
        /// The CHECK group to set the end of the searchable
        /// region for the CHECK-DAG rule
        right: Box<CheckGroup<'a>>,
    },
    /// A group of rules that must be matched consecutively,
    /// and must not overlap.
    Ordered(Vec<Box<dyn DynRule + 'a>>),
    /// An implicit group formed by a rule that is repeated N times
    Repeated {
        rule: Box<dyn DynRule + 'a>,
        count: usize,
    },
    /// A tree of patterns to match depth-first
    ///
    /// This group type occurs in the presence of CHECK-NOT directives
    Tree(Box<CheckTree<'a>>),
}
impl<'a> CheckGroup<'a> {
    pub fn first_pattern_span(&self) -> SourceSpan {
        match self {
            Self::Never(match_any) => match_any.first_pattern_span(),
            Self::Unordered(check_dag) => check_dag.first_pattern_span(),
            Self::Bounded { left, .. } => left.first_pattern_span(),
            Self::Ordered(rules) => rules
                .iter()
                .map(|rule| rule.span())
                .min_by_key(|span| span.start())
                .unwrap(),
            Self::Repeated { rule, .. } => rule.span(),
            Self::Tree(tree) => match tree.leftmost() {
                Left(group) => group.first_pattern_span(),
                Right(match_any) => match_any.first_pattern_span(),
            },
        }
    }

    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Never(match_any) => match_any.span(),
            Self::Unordered(check_dag) => check_dag.span(),
            Self::Bounded { left, right } => {
                let start = left.span().start();
                let end = right.span().end();
                SourceSpan::from(start..end)
            }
            Self::Ordered(rules) => {
                let start = rules[0].span().start();
                let end = rules.last().unwrap().span().end();
                SourceSpan::from(start..end)
            }
            Self::Repeated { rule, .. } => rule.span(),
            Self::Tree(ref tree) => {
                let leftmost_start = match tree.leftmost() {
                    Left(left) => left.span().start(),
                    Right(left) => left.span().start(),
                };
                let rightmost_end = match tree.rightmost() {
                    Left(right) => right.span().end(),
                    Right(right) => right.span().end(),
                };
                SourceSpan::from(leftmost_start..rightmost_end)
            }
        }
    }
}

#[derive(Debug)]
pub enum CheckSection<'a> {
    /// Rules contained between two CHECK-LABEL directives, or between a CHECK-LABEL
    /// directive and the end of the input, whichever comes first.
    ///
    /// The bounds of a block are known before evaluating the rules it contains,
    /// so all searches in a block are automatically bounded to that block.
    Block {
        /// The input span which defines the bounds for searches in the block
        label: SimpleMatcher<'a>,
        body: Vec<CheckGroup<'a>>,
    },
    /// Rules which are part of a single logical group
    ///
    /// Groups are formed in one of the following ways:
    ///
    /// * Rules following a CHECK or CHECK-COUNT directive belong
    /// to the same logical group, until a CHECK-NOT, CHECK-DAG,
    /// or the next CHECK/CHECK-COUNT/CHECK-LABEL
    ///
    /// * A set of CHECK-NOT rules is its own special type of group,
    /// see the Exclude* variants for details.
    ///
    /// * A set of CHECK-DAG rules is its own special type of group,
    /// see the Unordered* variants for details.
    Group { body: CheckGroup<'a> },
}

#[derive(Default, Debug)]
pub struct CheckProgram<'a> {
    pub sections: Vec<CheckSection<'a>>,
}
impl<'a> CheckProgram<'a> {
    pub fn compile(
        check_file: CheckFile<'a>,
        config: &Config,
        interner: &mut StringInterner,
    ) -> DiagResult<Self> {
        let lines = check_file.into_lines();
        if lines.is_empty() {
            return Err(Report::from(InvalidCheckFileError::Empty));
        }

        let mut program = Self::default();
        program.compile_lines(lines, config, interner)?;

        Ok(program)
    }

    /// Preprocess lines into blocks/groups
    fn compile_lines(
        &mut self,
        lines: Vec<CheckLine<'a>>,
        config: &Config,
        interner: &mut StringInterner,
    ) -> DiagResult<()> {
        // Divide up input lines into blocks
        let mut iter = lines.into_iter().peekable();
        let mut label = None;
        let mut block = vec![];
        let mut blocks: VecDeque<(Option<CheckLine<'a>>, Vec<CheckLine<'a>>)> = VecDeque::default();
        while let Some(next) = iter.peek() {
            match next.kind() {
                Check::Label => {
                    if !block.is_empty() {
                        blocks.push_back((label.take(), core::mem::take(&mut block)));
                    }
                    label = iter.next();
                    while let Some(next) = iter.peek() {
                        match next.kind() {
                            Check::Label => {
                                break;
                            }
                            _ => {
                                block.push(iter.next().unwrap());
                            }
                        }
                    }
                }
                Check::Empty | Check::Same | Check::Next if block.is_empty() && label.is_none() => {
                    return Err(Report::from(InvalidCheckFileError::InvalidFirstCheck {
                        kind: Check::Empty,
                        line: next.span(),
                    }));
                }
                Check::Plain
                | Check::Count(_)
                | Check::Next
                | Check::Same
                | Check::Not
                | Check::Dag
                | Check::Empty => {
                    block.push(iter.next().unwrap());
                }
                _ => unreachable!(),
            }
        }

        if !block.is_empty() {
            blocks.push_back((label.take(), block));
        }

        self.compile_blocks(&mut blocks, config, interner)
    }

    /// Categorize and process blocks
    fn compile_blocks(
        &mut self,
        blocks: &mut VecDeque<(Option<CheckLine<'a>>, Vec<CheckLine<'a>>)>,
        config: &Config,
        interner: &mut StringInterner,
    ) -> DiagResult<()> {
        let mut groups = vec![];
        let mut pending_tree = None;

        while let Some((maybe_label, body)) = blocks.pop_front() {
            let mut body = VecDeque::from(body);
            while let Some(line) = body.pop_front() {
                match line.kind() {
                    Check::Not => {
                        assert!(pending_tree.is_none());
                        let mut nots = vec![line];
                        while let Some(next) = body.pop_front() {
                            if matches!(next.kind(), Check::Not) {
                                nots.push(next);
                            } else {
                                body.push_front(next);
                                break;
                            }
                        }
                        let matcher = MatchAny::from(MatchAll::compile(nots, config, interner)?);
                        if body.is_empty() {
                            match groups.pop() {
                                Some(CheckGroup::Tree(left)) => {
                                    groups.push(CheckGroup::Tree(Box::new(CheckTree::Left {
                                        root: matcher,
                                        left,
                                    })))
                                }
                                Some(left @ CheckGroup::Unordered(_)) => {
                                    groups.push(CheckGroup::Tree(Box::new(CheckTree::Left {
                                        root: matcher,
                                        left: Box::new(CheckTree::Leaf(left)),
                                    })))
                                }
                                Some(group) => {
                                    groups.push(group);
                                    groups.push(CheckGroup::Never(matcher));
                                }
                                None => groups.push(CheckGroup::Never(matcher)),
                            }
                        } else {
                            match groups.pop() {
                                Some(CheckGroup::Tree(left)) => {
                                    pending_tree = Some((Some(left), matcher));
                                }
                                Some(left @ CheckGroup::Unordered(_)) => {
                                    let left = Box::new(CheckTree::Leaf(left));
                                    pending_tree = Some((Some(left), matcher));
                                }
                                Some(group) => {
                                    groups.push(group);
                                    pending_tree = Some((None, matcher));
                                }
                                None => {
                                    pending_tree = Some((None, matcher));
                                }
                            }
                        }
                    }
                    Check::Dag => {
                        let mut dags = vec![line];
                        while let Some(next) = body.pop_front() {
                            if matches!(next.kind(), Check::Dag) {
                                dags.push(next);
                            } else {
                                body.push_front(next);
                                break;
                            }
                        }
                        let check_dag =
                            Box::new(CheckDag::new(MatchAll::compile(dags, config, interner)?));

                        let group = if matches!(
                            body.front().map(|line| line.kind()),
                            Some(Check::Plain | Check::Count(_))
                        ) {
                            let line = body.pop_front().unwrap();
                            let bounding_group = match line.kind() {
                                Check::Plain => CheckGroup::Ordered(vec![self.compile_rule(
                                    line.ty,
                                    line.pattern,
                                    config,
                                    interner,
                                )?]),
                                Check::Count(count) => CheckGroup::Repeated {
                                    rule: self.compile_rule(
                                        line.ty,
                                        line.pattern,
                                        config,
                                        interner,
                                    )?,
                                    count,
                                },
                                _ => unsafe { std::hint::unreachable_unchecked() },
                            };
                            CheckGroup::Bounded {
                                left: check_dag,
                                right: Box::new(bounding_group),
                            }
                        } else {
                            CheckGroup::Unordered(check_dag)
                        };
                        if let Some((maybe_left, root)) = pending_tree.take() {
                            match maybe_left {
                                Some(left) => {
                                    groups.push(CheckGroup::Tree(Box::new(CheckTree::Both {
                                        root,
                                        left,
                                        right: Box::new(CheckTree::Leaf(group)),
                                    })));
                                }
                                None => {
                                    groups.push(CheckGroup::Tree(Box::new(CheckTree::Right {
                                        root,
                                        right: Box::new(CheckTree::Leaf(group)),
                                    })));
                                }
                            }
                        } else {
                            groups.push(group);
                        }
                    }
                    Check::Count(count) => {
                        let group = CheckGroup::Repeated {
                            rule: self.compile_rule(line.ty, line.pattern, config, interner)?,
                            count,
                        };
                        if let Some((maybe_left, root)) = pending_tree.take() {
                            match maybe_left {
                                Some(left) => {
                                    groups.push(CheckGroup::Tree(Box::new(CheckTree::Both {
                                        root,
                                        left,
                                        right: Box::new(CheckTree::Leaf(group)),
                                    })));
                                }
                                None => {
                                    groups.push(CheckGroup::Tree(Box::new(CheckTree::Right {
                                        root,
                                        right: Box::new(CheckTree::Leaf(group)),
                                    })));
                                }
                            }
                        } else {
                            groups.push(group);
                        }
                    }
                    _ => {
                        body.push_front(line);
                        let mut rules: Vec<Box<dyn DynRule + 'a>> = vec![];
                        while let Some(next) = body.pop_front() {
                            match next.kind() {
                                Check::Not | Check::Dag | Check::Count(_) => {
                                    body.push_front(next);
                                    break;
                                }
                                Check::Empty => {
                                    rules.push(Box::new(CheckEmpty::new(next.span())));
                                }
                                _ => {
                                    rules.push(self.compile_rule(
                                        next.ty,
                                        next.pattern,
                                        config,
                                        interner,
                                    )?);
                                }
                            }
                        }
                        let group = CheckGroup::Ordered(rules);
                        if let Some((maybe_left, root)) = pending_tree.take() {
                            match maybe_left {
                                Some(left) => {
                                    groups.push(CheckGroup::Tree(Box::new(CheckTree::Both {
                                        root,
                                        left,
                                        right: Box::new(CheckTree::Leaf(group)),
                                    })));
                                }
                                None => {
                                    groups.push(CheckGroup::Tree(Box::new(CheckTree::Right {
                                        root,
                                        right: Box::new(CheckTree::Leaf(group)),
                                    })));
                                }
                            }
                        } else {
                            groups.push(group);
                        }
                    }
                }
            }
            assert!(pending_tree.is_none());
            if let Some(label) = maybe_label {
                let label = Pattern::compile_static(label.span, label.pattern, config, interner)?;
                self.sections.push(CheckSection::Block {
                    label,
                    body: core::mem::take(&mut groups),
                });
            } else {
                for body in core::mem::take(&mut groups).into_iter() {
                    self.sections.push(CheckSection::Group { body });
                }
            }
        }

        Ok(())
    }

    fn compile_rule(
        &mut self,
        ty: CheckType,
        pattern: CheckPattern<'a>,
        config: &Config,
        interner: &mut StringInterner,
    ) -> DiagResult<Box<dyn DynRule + 'a>> {
        let pattern = if ty.is_literal_match() {
            Pattern::compile_literal(pattern, config)?
        } else {
            Pattern::compile(pattern, config, interner)?
        };

        match ty.kind {
            Check::Plain | Check::Count(_) => {
                Ok(Box::new(CheckPlain::new(pattern.into_matcher_mut())))
            }
            Check::Same => Ok(Box::new(CheckSame::new(pattern.into_matcher_mut()))),
            Check::Next => Ok(Box::new(CheckNext::new(pattern.into_matcher_mut()))),
            kind => unreachable!("we should never be compiling a rule for {kind} here"),
        }
    }
}
