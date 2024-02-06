use crate::{check::CheckProgram, common::*};

use super::{CheckPattern, CheckType};

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

    pub fn compile(
        self,
        config: &Config,
        interner: &mut StringInterner,
    ) -> DiagResult<CheckProgram<'a>> {
        CheckProgram::compile(self, config, interner)
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

    pub fn is_regex_compatible(&self) -> bool {
        self.pattern.is_regex_compatible()
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
