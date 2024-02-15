use std::ops::RangeBounds;

use litcheck::{
    range::{self, Range},
    text,
};

#[derive(Debug, Copy, Clone)]
pub struct Input<'a> {
    buffer: &'a [u8],
    span: Range<usize>,
    anchored: bool,
    crlf: bool,
}
impl<'a> Input<'a> {
    #[inline]
    pub fn new(buffer: &'a [u8], crlf: bool) -> Self {
        Self {
            buffer,
            span: Range::from(0..buffer.len()),
            anchored: false,
            crlf,
        }
    }

    pub fn anchored(mut self, yes: bool) -> Self {
        self.anchored = yes;
        self
    }

    pub fn span<R: RangeBounds<usize>>(mut self, range: R) -> Self {
        let range = range::range_from_bounds(range, Range::new(0, self.buffer.len()))
            .expect("invalid range");
        self.span = range;
        self
    }

    pub fn is_anchored(&self) -> bool {
        self.anchored
    }

    pub fn is_crlf(&self) -> bool {
        self.crlf
    }

    /// Return the starting index of this search
    pub fn start(&self) -> usize {
        self.span.start
    }

    /// Return the starting index of this search
    pub fn end(&self) -> usize {
        self.span.end
    }

    /// Set whether searches of this input should be anchored
    pub fn set_anchored(&mut self, anchored: bool) {
        self.anchored = anchored;
    }

    /// Set the start of the search area to `start`
    ///
    /// `start` is clamped to the end of the buffer.
    pub fn set_start(&mut self, start: usize) {
        self.span.start = core::cmp::min(start, self.buffer.len());
    }

    /// Set the bounds of the search area to `range`
    ///
    /// The range is clamped to the buffer size
    pub fn set_span(&mut self, range: Range<usize>) {
        let eof = self.buffer.len();
        self.span.start = core::cmp::min(range.start, eof);
        self.span.end = core::cmp::min(range.end, eof);
    }

    /// Returns true if this search could never return any other match
    ///
    /// Specifically, if the start position of the search is greater than the end
    pub fn is_empty(&self) -> bool {
        self.span.is_empty()
    }

    /// Returns the boundaries to which any searches of this input
    /// will be constrained as a [Range].
    pub fn bounds(&self) -> Range<usize> {
        self.span
    }

    /// Returns the original underlying buffer
    pub fn buffer(&self) -> &'a [u8] {
        self.buffer
    }

    /// Returns the portion of the underlying buffer which is searchable
    pub fn as_slice(&self) -> &[u8] {
        &self.buffer[self.span]
    }

    /// Returns the specified portion of the underlying buffer as a `str`
    ///
    /// This function will panic if the range is invalid, the range does not
    /// start on a valid utf-8 codepoint boundary, or the range is not valid
    /// utf-8.
    pub fn as_str<R>(&self, range: R) -> &'a str
    where
        R: RangeBounds<usize>,
    {
        let range = range::range_from_bounds(range, Range::new(0, self.buffer.len()))
            .expect("invalid range");
        assert!(
            text::is_char_boundary(self.buffer, range.start),
            "a str must start on a valid utf-8 codepoint boundary"
        );
        core::str::from_utf8(&self.buffer[range]).expect("buffer is not valid utf-8")
    }

    /// Find the next newline from the current position, up to the end of the current search
    pub fn next_newline(&self) -> Option<usize> {
        self.next_newline_in_range(self.span)
    }

    /// Find the previous newline from the current position, up to the end of the current search
    pub fn prev_newline(&self) -> Option<usize> {
        self.prev_newline_in_range(self.span)
    }

    /// Find the next newline from the given offset
    pub fn next_newline_from(&self, offset: usize) -> Option<usize> {
        self.next_newline_in_range(offset..)
    }

    /// Find the previous newline, staring from from the given offset
    pub fn prev_newline_from(&self, offset: usize) -> Option<usize> {
        self.prev_newline_in_range(..offset)
    }

    /// Find the next newline in the given range
    pub fn next_newline_in_range<R>(&self, range: R) -> Option<usize>
    where
        R: RangeBounds<usize>,
    {
        let range = range::range_from_bounds(range, Range::new(0, self.buffer.len())).unwrap();
        if self.crlf {
            text::find_next_crlf_or_eof(self.buffer, range)
        } else {
            text::find_next_lf_or_eof(self.buffer, range)
        }
    }

    /// Find the previous newline in the given range, starting from the end of the range
    pub fn prev_newline_in_range<R>(&self, range: R) -> Option<usize>
    where
        R: RangeBounds<usize>,
    {
        let range = range::range_from_bounds(range, Range::new(0, self.buffer.len())).unwrap();
        if self.crlf {
            text::find_prev_crlf_or_eof(self.buffer, range)
        } else {
            text::find_prev_lf_or_eof(self.buffer, range)
        }
    }

    pub fn pop_byte(&mut self) -> (usize, u8) {
        if self.is_empty() {
            (self.span.end, b'\0')
        } else {
            let offset = self.span.start;
            let byte = self.buffer[offset];
            self.span.start = core::cmp::min(offset + 1, self.span.end);
            (offset, byte)
        }
    }

    pub fn peek_byte(&mut self) -> (usize, u8) {
        if self.is_empty() {
            (self.span.end, b'\0')
        } else {
            let offset = self.span.start;
            let byte = self.buffer[offset];
            (offset, byte)
        }
    }

    pub fn pop(&mut self) -> (usize, char) {
        let (next, len) = bstr::decode_utf8(self.as_slice());
        match next {
            Some(c) => {
                let offset = self.span.start;
                self.span.start += len;
                (offset, c)
            }
            None => (self.span.start, '\0'),
        }
    }

    pub fn peek(&self) -> char {
        let (next, _) = bstr::decode_utf8(self.as_slice());
        next.unwrap_or('\0')
    }
}
impl<'a> From<Input<'a>> for regex_automata::Input<'a> {
    fn from(input: Input<'a>) -> regex_automata::Input<'a> {
        use regex_automata::Anchored;
        let anchored = if input.anchored {
            Anchored::Yes
        } else {
            Anchored::No
        };
        regex_automata::Input::new(input.buffer)
            .anchored(anchored)
            .range(input.span)
    }
}
impl<'a> From<Input<'a>> for aho_corasick::Input<'a> {
    fn from(input: Input<'a>) -> aho_corasick::Input<'a> {
        use aho_corasick::Anchored;
        let anchored = if input.anchored {
            Anchored::Yes
        } else {
            Anchored::No
        };
        aho_corasick::Input::new(input.buffer)
            .anchored(anchored)
            .range(input.span)
    }
}
