enum PatternTreeNode {
    /// There is only one pattern that can match at this
    /// point, so the entire tree is a match if `pattern`
    /// matches
    Leaf {
        pattern: PatternId,
    },
    /// There is overlap between multiple patterns at this
    /// point, so match the common prefix, and then choose
    /// which node to continue matching with based on the
    /// next character of the input following the prefix
    Overlapping {
        prefix: PatternId,
        suffixes: Vec<(Selector, PatternId, NodeId)>,
        /// This is true when `prefix` itself is a valid match
        is_prefix_match: bool,
    },
    /// There is no overlap between patterns at this point,
    /// so we can dispatch based on the next character to one of
    /// `patterns`
    Distinct {
        prefixes: Vec<(Selector, PatternId, NodeId)>,
    }
}

type NodeId = usize;

#[derive(Default)]
pub struct PatternTree<'a> {
    nodes: Vec<PatternTreeNode>,
    patterns: Vec<RegexPatternPart<'a>>,
    root: NodeId,
}

#[derive(Default)]
pub struct PatternTreeBuilder<'a> {
    tree: PatternTree<'a>,
    overlapping: DelayedPatternSet<'a>,
    current_node: NodeId,
}
impl<'a> PatternTreeBuilder<'a> {
    pub fn current_node(&self) -> NodeId {
        self.current_node
    }

    pub fn switch_to_node(&mut self, node: NodeId) {
        self.current_node = node;
    }

    /// Add `pattern` to the set of patterns which are expected to match at the current point
    pub fn match_at_current(&mut self, pattern: RegexPatternPart<'a>) -> NodeId {
        if self.tree.nodes.is_empty() {
            self.tree.patterns.push(pattern);
            self.tree.nodes.push(PatternTreeNode::Leaf { pattern: 0 });
            return;
        }

        match &mut self.tree.nodes[self.current_node] {
            PatternTreeNode::Leaf { pattern: prefix_id } => {
                let prefix_id = *prefix_id;
                let prefix = core::mem::replace(&mut self.tree.patterns[prefix_id], RegexPatternPart::Literal(Cow::Borrowed("")));
                match RegexPatternPart::combine(prefix, pattern) {
                    PatternCombo::Identical(pattern) => {
                        self.tree.patterns[prefix_id] = pattern;
                    }
                    PatternCombo::Overlapping { prefix, suffixes } => {
                        self.tree.patterns[prefix_id] = prefix;
                        let mut overlapping = vec![];
                        for (selector, suffix) in suffixes.into_iter() {
                            let suffix_id = self.tree.patterns.len();
                            let node_id = self.tree.nodes.len();
                            overlapping.push((selector, suffix_id, node_id));
                            self.tree.nodes.push(PatternTreeNode::Leaf { pattern: suffix_id });
                            self.tree.patterns.push(suffix);
                        }
                        self.tree.nodes[self.current_node] = PatternTreeNode::Overlapping { prefix: prefix_id, suffixes: overlapping };
                    }
                    PatternCombo::Distinct { prefixes } => {
                        let mut distinct = vec![];
                        for (selector, suffix) in prefixes.into_iter() {
                            let suffix_id = self.tree.patterns.len();
                            let node_id = self.tree.nodes.len();
                            distinct.push((selector, suffix_id, node_id));
                            self.tree.nodes.push(PatternTreeNode::Leaf { pattern: suffix_id });
                            self.tree.patterns.push(suffix);
                        }
                        self.tree.nodes[self.current_node] = PatternTreeNode::Distinct { prefixes: distinct };
                    }
                }
            }
            PatternTreeNode::Overlapping { prefix: prefix_id, ref mut is_prefix_match, suffixes: ref overlapping } => {
                let prefix_id = *prefix_id;
                match self.tree.patterns[prefix_id].overlap(&pattern) {
                    Some(Overlap::Exact) => {
                        *is_prefix_match = true;
                    }
                    Some(Overlap::Partial() => {
                    }
                    None => {
                        let a = self.tree.patterns[prefix_id].selector();
                        let b = pattern.selector();
                        let pattern_id = self.tree.patterns.len();
                        self.tree.patterns.push(pattern_id);
                        let overlapping_node_id = self.tree.nodes.len();
                        let node_id = self.tree.nodes.len();
                        let overlapping = core::mem::replace(&mut self.tree.nodes[self.current_node], PatternTreeNode::Distinct { prefixes: vec![(a, prefix_id, overlapping_node_id), (b, pattern_id, node_id)] });
                        self.tree.nodes.push(overlapping);
                        self.tree.nodes.push(PatternTreeNode::Leaf { pattern: pattern_id });
                    }
                }
            }
        }
        self.overlapping.push(pattern);
    }

    pub fn build(self) -> DiagResult<PatternTree<'a>> {
        Ok(self.tree)
    }
}
