mod display;
pub use self::display::DisplayCommaSeparated;

use std::borrow::Cow;

use crate::range::Range;

/// The style of line endings used in a file
#[derive(Debug, Copy, Clone)]
pub enum LineEnding {
    /// The line ending was `\n`
    Lf,
    /// The line ending was `\r\n`
    Crlf,
    /// No line ending was found before EOF
    None,
}

/// Represents a newline location and style
#[derive(Debug, Copy, Clone)]
pub struct Newline {
    pub offset: usize,
    pub ty: LineEnding,
}
impl Newline {
    #[inline(always)]
    pub fn offset(&self) -> usize {
        self.offset
    }

    #[inline]
    pub fn next_line_start(&self) -> usize {
        match self.ty {
            LineEnding::Lf => self.offset + 1,
            LineEnding::Crlf => self.offset + 2,
            LineEnding::None => self.offset,
        }
    }

    #[inline]
    pub fn is_crlf(&self) -> bool {
        matches!(self.ty, LineEnding::Crlf)
    }

    /// Find the next [Newline] from the start of `buffer`
    pub fn next(buffer: &[u8]) -> Self {
        Self::next_from(buffer, 0)
    }

    /// Find the [Newline] from `offset` in `buffer`
    ///
    /// If there are no more newlines, the newline index will be equal
    /// to `buffer.len()`, i.e. end of file.
    pub fn next_from(buffer: &[u8], offset: usize) -> Self {
        match memchr::memchr(b'\n', &buffer[offset..]) {
            Some(index) => {
                let index = offset + index;
                if index > 0 {
                    let line_end = index - 1;
                    match unsafe { *buffer.get_unchecked(line_end) } {
                        b'\r' => Self {
                            ty: LineEnding::Crlf,
                            offset: line_end,
                        },
                        _ => Self {
                            ty: LineEnding::Lf,
                            offset: index,
                        },
                    }
                } else {
                    Self {
                        ty: LineEnding::Lf,
                        offset: index,
                    }
                }
            }
            None => Self {
                ty: LineEnding::None,
                offset: buffer.len(),
            },
        }
    }

    /// Find the last [Newline] in `buffer`
    pub fn prev(buffer: &[u8]) -> Self {
        Self::prev_from(buffer, 0)
    }

    /// Find the last [Newline] in `buffer`, searching backwards from `offset`.
    ///
    /// If there are no newlines, the newline offset returned will be zero,
    /// i.e. beginning of file. Same if the buffer is empty.
    pub fn prev_from(buffer: &[u8], offset: usize) -> Self {
        match memchr::memrchr(b'\n', &buffer[..offset]) {
            Some(index) => {
                if index > 0 {
                    let prev_line_end = index - 1;
                    match unsafe { *buffer.get_unchecked(prev_line_end) } {
                        b'\r' => Self {
                            ty: LineEnding::Crlf,
                            offset: prev_line_end,
                        },
                        _ => Self {
                            ty: LineEnding::Crlf,
                            offset: index,
                        },
                    }
                } else {
                    Self {
                        ty: LineEnding::Lf,
                        offset: index,
                    }
                }
            }
            None => Self {
                ty: LineEnding::None,
                offset: 0,
            },
        }
    }
}

pub fn find_next_lf_or_eof(buffer: &[u8], range: Range<usize>) -> Option<usize> {
    memchr::memchr(b'\n', &buffer[range]).map(|idx| range.start + idx)
}

pub fn find_next_crlf_or_eof(buffer: &[u8], range: Range<usize>) -> Option<usize> {
    match memchr::memchr(b'\n', &buffer[range]) {
        Some(0) => None,
        Some(index) => {
            let line_end = index - 1;
            match unsafe { *buffer.get_unchecked(line_end) } {
                b'\r' => Some(line_end),
                _ => None,
            }
        }
        None => None,
    }
}

pub fn find_prev_lf_or_eof(buffer: &[u8], range: Range<usize>) -> Option<usize> {
    memchr::memrchr(b'\n', &buffer[range]).map(|idx| range.start + idx)
}

pub fn find_prev_crlf_or_eof(buffer: &[u8], range: Range<usize>) -> Option<usize> {
    match memchr::memrchr(b'\n', &buffer[range]) {
        Some(0) => None,
        Some(index) => {
            let line_end = index - 1;
            match unsafe { *buffer.get_unchecked(line_end) } {
                b'\r' => Some(line_end),
                _ => None,
            }
        }
        None => None,
    }
}

/// Returns true if `offset` in the underlying buffer falls on a valid UTF-8 codepoint boundary.
///
/// This is only specified if the input buffer is valid UTF-8, no guarantees otherwise.
pub fn is_char_boundary(buffer: &[u8], offset: usize) -> bool {
    // NOTE: This is inlined from regex_automata/src/util/utf8.rs (is_boundary)
    // which is dual-licensed Apache 2.0/MIT.
    match buffer.get(offset) {
        // The end of the buffer is technically a valid boundary
        None => offset == buffer.len(),
        // Other than ASCII (where the most significant bit is never set),
        // valid starting bytes always have their most significant two bits
        // set, where as continuation bytes never have their second most
        // significant bit set. Therefore, this only returns true when bytes[i]
        // corresponds to a byte that begins a valid UTF-8 encoding of a
        // Unicode scalar value.
        Some(&b) => b <= 0b0111_1111 || b >= 0b1100_0000,
    }
}

pub fn canonicalize_horizontal_whitespace(
    s: Cow<'_, str>,
    strict_whitespace: bool,
) -> Cow<'_, str> {
    if strict_whitespace {
        return s;
    }

    if s.contains(is_non_canonical_horizontal_whitespace) {
        Cow::Owned(s.replace(is_non_canonical_horizontal_whitespace, " "))
    } else {
        s
    }
}

#[inline]
fn is_non_canonical_horizontal_whitespace(c: char) -> bool {
    match c {
        '\t' => true,
        // Unicode Space_Separator category, sans space (which we are canonicalizing to)
        '\u{00A0}'
        | '\u{1680}'
        | '\u{2000}'..='\u{200A}'
        | '\u{202F}'
        | '\u{205F}'
        | '\u{3000}' => true,
        _ => false,
    }
}
