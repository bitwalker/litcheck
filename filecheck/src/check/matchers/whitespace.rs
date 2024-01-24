use std::fmt;

use litcheck::diagnostics::{SourceSpan, Spanned};

use super::*;
use crate::check::Input;

/// This matcher accepts only ASCII whitespace characters,
/// and is always anchored, i.e. it will never match input
/// that starts with a non-ASCII whitespace character.
pub struct AsciiWhitespaceMatcher {
    span: SourceSpan,
}
impl fmt::Debug for AsciiWhitespaceMatcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("AsciiWhitespaceMatcher")
            .field("span", &self.span)
            .finish()
    }
}
impl AsciiWhitespaceMatcher {
    pub fn new(span: SourceSpan) -> Self {
        Self { span }
    }
}
impl Matcher for AsciiWhitespaceMatcher {
    fn try_match<'input>(&self, input: Input<'input>) -> Option<MatchInfo<'input>> {
        let buffer = input.as_slice();
        if input.is_empty() || !buffer[0].is_ascii_whitespace() {
            return None;
        }

        let start = input.start();
        let mut len = 0;
        for c in buffer.iter() {
            if !c.is_ascii_whitespace() {
                break;
            }
            len += 1;
        }

        Some(MatchInfo::new(
            SourceSpan::from(start..(start + len)),
            self.span,
        ))
    }
}
impl Spanned for AsciiWhitespaceMatcher {
    fn span(&self) -> SourceSpan {
        self.span
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_whitespace_matcher() {
        let mut input = String::new();
        input.push('\t');
        input.push('\r');
        input.push(' ');
        input.push('\n');
        input.push_str("abc");
        input.push('\n');

        let matcher = AsciiWhitespaceMatcher::new(SourceSpan::from(0..0));
        let bytes = input.as_bytes();
        let input = Input::new(bytes, false).span(0..);
        let result = matcher.try_match(input);
        let info = result.expect("expected match");
        assert_eq!(info.span.offset(), 0);
        assert_eq!(info.span.len(), 4);
    }
}
