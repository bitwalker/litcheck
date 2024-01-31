use std::ops::{Deref, DerefMut, RangeBounds};

use crate::common::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CursorPosition {
    pub range: Range<usize>,
    pub eol: u32,
}

/// A [Cursor] is a pointer to a byte in a buffer, with additional
/// capabilities and constraints:
///
/// * The full underlying buffer can be accessed
/// * The cursor itself can be constrained to a sub-range of the buffer
/// * The current end-of-line position is made available
/// * Look-around assertions, like in regular expressions, can be queried
/// from the current cursor position
/// * A searchable [Input] can be started from a cursor
#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    /// The input buffer being checked
    pub buffer: &'a [u8],
    /// The byte range of the current block.
    ///
    /// Any searches will implicitly stop at the end of this range as if it is EOF.
    ///
    /// Defaults to the full span of the input buffer.
    block: Range<usize>,
    /// The index at which the current line ends
    eol: u32,
    /// A look-around matcher for the current input
    look: LookMatcher,
    /// Returns true if the buffer uses CRLF line-endings
    crlf: bool,
}
impl<'a> Cursor<'a> {
    pub fn new(buffer: &'a [u8]) -> Self {
        let newline = Newline::next(buffer);
        let eof = buffer.len();
        Self {
            buffer,
            block: Range::new(0, eof),
            eol: newline.offset().try_into().expect("line too large"),
            look: LookMatcher::new(),
            crlf: newline.is_crlf(),
        }
    }

    #[inline]
    pub fn protect<'guard, 'cursor: 'guard>(&'cursor mut self) -> CursorGuard<'guard, 'a> {
        CursorGuard::new(self)
    }

    #[inline(always)]
    pub fn buffer(&self) -> &'a [u8] {
        self.buffer
    }

    /// Returns true if the underlying buffer uses CRLF line endings
    #[inline(always)]
    pub fn is_crlf(&self) -> bool {
        self.crlf
    }

    /// Returns true if the cursor range is empty
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.block.is_empty()
    }

    /// Returns the current position information for this cursor
    #[inline]
    pub fn position(&self) -> CursorPosition {
        CursorPosition {
            range: self.block,
            eol: self.eol,
        }
    }

    /// Get the current range of this cursor as a [SourceSpan]
    pub fn span(&self) -> SourceSpan {
        SourceSpan::from(self.block)
    }

    /// Get the current range of this cursor
    pub fn range(&self) -> Range<usize> {
        self.block
    }

    /// Returns the index corresponding to the start of the cursor range
    #[inline(always)]
    pub fn start(&self) -> usize {
        self.block.start
    }

    /// Returns the index corresponding to the end of the cursor range
    #[inline(always)]
    pub fn end(&self) -> usize {
        self.block.end
    }

    /// Returns the index (exclusive) corresponding to the end of the input buffer
    ///
    /// The returned index is valid for use in a range, but cannot be used to index the buffer.
    pub fn end_of_file(&self) -> usize {
        self.buffer.len()
    }

    /// Return the index corresponding to the next newline in the input
    ///
    /// This will be equal to `self.end_of_file()` if there are no more newlines in the input.
    #[inline(always)]
    pub fn end_of_line(&self) -> usize {
        self.eol as usize
    }

    /// Reset this cursor to the start of the underlying buffer, and
    /// extend the bounds to cover the entire buffer, i.e. the initial
    /// state of the cursor.
    pub fn reset(&mut self) {
        let newline = Newline::next(self.buffer);
        self.eol = newline.offset() as u32;
        self.block = Range::new(0, self.buffer.len());
    }

    /// Set the start position of the cursor, as long as it is within the current cursor range
    pub fn set_start(&mut self, start: usize) {
        use core::cmp::Ordering;

        let prev_start = self.block.start;
        self.block.start = core::cmp::min(start, self.block.end);
        match start.cmp(&prev_start) {
            Ordering::Greater => {
                if self.block.start > self.eol as usize {
                    let eol = self
                        .next_newline_in_range(self.block)
                        .unwrap_or(self.block.end);
                    self.eol = eol as u32;
                }
            }
            Ordering::Less => {
                let eol = self
                    .next_newline_in_range(self.block)
                    .unwrap_or(self.block.end);
                self.eol = eol as u32;
            }
            Ordering::Equal => (),
        }
    }

    /// Move the cursor to `pos`
    pub fn move_to(&mut self, pos: CursorPosition) {
        self.block = pos.range;
        self.eol = pos.eol;
    }

    /// Move the cursor start to the end of its range
    pub fn move_to_end(&mut self) {
        self.block.start = self.block.end;
        self.eol = self.block.end as u32;
    }

    /// Set the bounds for this cursor
    ///
    /// The cursor will never go beyond these bounds, even when explicitly requested
    pub fn set_bounds<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        let eof = self.buffer.len();
        self.block = range::range_from_bounds(range, Range::new(0, eof)).unwrap();
        self.eol = self.next_newline_from(self.block.start).unwrap_or(eof) as u32;
    }

    /// Move the cursor forward past the newline under the cursor.
    ///
    /// If the cursor is not on a newline, this will panic.
    ///
    /// If the move would place the cursor out of bounds, it is
    /// stopped at the end of the cursor range instead.
    pub fn consume_newline(&mut self) {
        debug_assert!(
            self.is_end_of_line(self.block.start),
            "expected newline (crlf={}) at offset {}, invalid sequence starts with character code {}",
            self.is_crlf(),
            self.block.start,
            self.buffer[self.block.start]
        );
        let start = self.start_of_next_line();
        self.block.start = core::cmp::min(start, self.block.end);
    }

    /// Return the index corresponding to the first byte following the next newline in the input
    ///
    /// This will be equal to `self.end_of_file()` if there are no more newlines in the input.
    ///
    /// This may return an index outside the current cursor range.
    pub fn start_of_next_line(&self) -> usize {
        Newline::next_from(self.buffer, self.block.start).next_line_start()
    }

    /// Get an [Input] that can be used to search the entire underlying buffer
    pub fn search(&self) -> Input<'a> {
        Input::new(self.buffer, self.is_crlf())
    }

    /// Get an [Input] that can be used to search from the current position to the
    /// end of the underlying buffer, ignoring the end bound of the cursor.
    pub fn search_to_end(&self) -> Input<'a> {
        Input::new(self.buffer, self.is_crlf()).span(self.block.start..)
    }

    /// Get an [Input] that can be used to search an arbitrary range of the underlying buffer
    pub fn search_range<R: RangeBounds<usize>>(&self, range: R) -> Input<'a> {
        Input::new(self.buffer, self.is_crlf()).span(range)
    }

    /// Get an [Input] that can be used to search the unvisited portion of the current block
    pub fn search_block(&self) -> Input<'a> {
        Input::new(self.buffer, self.is_crlf()).span(self.block)
    }

    /// Get an [Input] that can be used to search the unvisited portion of the current line
    pub fn search_line(&self) -> Input<'a> {
        Input::new(self.buffer, self.is_crlf()).span(self.block.start..self.end_of_line())
    }

    /// Returns true if the current position is at end of input
    pub fn at_eof(&self) -> bool {
        self.is_eof(self.block.start)
    }

    /// Returns true if `offset` in the input buffer is end of input
    pub fn is_eof(&self, offset: usize) -> bool {
        self.look.is_end(self.buffer, offset)
    }

    /// Returns true if the current position is at the start of a line
    pub fn at_start_of_line(&self) -> bool {
        self.is_start_of_line(self.block.start)
    }

    /// Returns true if `offset` in the input buffer is the start of a line (which includes the start of the input)
    ///
    /// The start of a line is immediately following a `\n` or `\r\n` (depending on crlf mode)
    pub fn is_start_of_line(&self, offset: usize) -> bool {
        if self.is_crlf() {
            self.look.is_start_crlf(self.buffer, offset)
        } else {
            self.look.is_start_lf(self.buffer, offset)
        }
    }

    /// Returns true if the current position is at the end of a line
    pub fn at_end_of_line(&self) -> bool {
        self.is_end_of_line(self.block.start)
    }

    /// Returns true if `offset` in the input buffer is the end of a line (which includes the end of the input)
    ///
    /// The end of a line is immediately preceding a `\n` or `\r\n` (depending on crlf mode)
    pub fn is_end_of_line(&self, offset: usize) -> bool {
        if self.is_crlf() {
            self.look.is_end_crlf(self.buffer, offset)
        } else {
            self.look.is_end_lf(self.buffer, offset)
        }
    }

    /// Find the next newline from the current position, up to the end of the current block
    pub fn next_newline(&self) -> Option<usize> {
        self.next_newline_in_range(self.block)
    }

    /// Find the previous newline from the current position, up to the end of the current block
    pub fn prev_newline(&self) -> Option<usize> {
        self.prev_newline_in_range(self.block)
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
}

pub struct CursorGuard<'a, 'input> {
    guarded: &'a mut Cursor<'input>,
    cursor: Cursor<'input>,
}
impl<'a, 'input> CursorGuard<'a, 'input> {
    pub fn new(guarded: &'a mut Cursor<'input>) -> Self {
        let cursor = guarded.clone();
        Self { guarded, cursor }
    }

    #[inline]
    pub fn save(self) {
        self.guarded.block = self.cursor.block;
        self.guarded.eol = self.eol;
    }
}
impl<'a, 'input> Deref for CursorGuard<'a, 'input> {
    type Target = Cursor<'input>;

    fn deref(&self) -> &Self::Target {
        &self.cursor
    }
}
impl<'a, 'input> DerefMut for CursorGuard<'a, 'input> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cursor
    }
}
impl<'a, 'input> AsRef<Cursor<'input>> for CursorGuard<'a, 'input> {
    fn as_ref(&self) -> &Cursor<'input> {
        &self.cursor
    }
}
impl<'a, 'input> AsMut<Cursor<'input>> for CursorGuard<'a, 'input> {
    fn as_mut(&mut self) -> &mut Cursor<'input> {
        &mut self.cursor
    }
}
