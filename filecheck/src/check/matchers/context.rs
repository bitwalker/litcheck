use std::{collections::BTreeMap, ops::RangeBounds};

use litcheck::{
    diagnostics::ArcSource,
    range::{self, Range},
    text::{self, Newline},
    StringInterner, Symbol,
};

use crate::{
    check::{matchers::LookMatcher, Input},
    expr::{Value, VariableName},
    Config,
};

#[derive(Debug, Copy, Clone)]
pub struct Cursor {
    eol: usize,
    block: Range<usize>,
}
impl Cursor {
    /// Returns the index of this cursor in the underlying buffer
    pub fn at(&self) -> usize {
        self.block.start
    }
}

pub struct MatchContext<'a> {
    /// The current global configuration
    pub config: &'a Config,
    /// The current string interner
    pub interner: &'a mut StringInterner,
    /// The input buffer being checked
    pub buffer: &'a [u8],
    pub match_file: ArcSource,
    pub input_file: ArcSource,
    /// The index at which the current line ends
    pub eol: usize,
    /// The byte range of the current block.
    ///
    /// Any searches will implicitly stop at the end of this range as if it is EOF.
    ///
    /// Defaults to the full span of the input buffer.
    pub block: Range<usize>,
    /// The operand stack used by some patterns
    pub stack: Vec<Value<'a>>,
    /// Global variables set on command line
    pub globals: BTreeMap<Symbol, Value<'a>>,
    /// Local variables bound during matching
    pub locals: BTreeMap<Symbol, Value<'a>>,
    /// A look-around matcher for the current input
    pub look: LookMatcher,
    /// Set to true if the current buffer uses CRLF line endings
    pub crlf: bool,
}
impl<'a> MatchContext<'a> {
    pub fn new(
        config: &'a Config,
        interner: &'a mut StringInterner,
        match_file: ArcSource,
        input_file: ArcSource,
        buffer: &'a [u8],
    ) -> Self {
        let newline = Newline::next(buffer);
        let eof = buffer.len();

        let globals = BTreeMap::from_iter(config.variables.iter().map(|v| {
            (
                interner.get_or_intern(v.name.as_ref()),
                match v.value {
                    Value::Undef => Value::Undef,
                    Value::Str(s) => Value::Str(s),
                    Value::Num(ref n) => Value::Num(n.clone()),
                },
            )
        }));

        Self {
            config,
            interner,
            match_file,
            input_file,
            buffer,
            eol: newline.offset(),
            block: Range::new(0, eof),
            stack: vec![],
            globals,
            locals: Default::default(),
            look: LookMatcher::new(),
            crlf: newline.is_crlf(),
        }
    }

    pub fn cursor(&self) -> Cursor {
        Cursor {
            eol: self.eol,
            block: self.block,
        }
    }

    pub fn reset_to_cursor(&mut self, cursor: Cursor) {
        self.eol = cursor.eol;
        self.block = cursor.block;
    }

    pub fn reset(&mut self) {
        let newline = Newline::next(self.buffer);
        self.eol = newline.offset();
        self.crlf = newline.is_crlf();
        self.block.start = 0;
        self.block.end = self.buffer.len();
        self.stack.clear();
        self.locals.clear();
    }

    pub fn enter_block<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        let eof = self.buffer.len();
        let range = range::range_from_bounds(range, Range::new(0, eof)).unwrap();
        let eol = self.next_newline_from(range.start).unwrap_or(eof);
        self.block = range;
        self.eol = eol;
        if self.config.enable_var_scope {
            self.locals.clear();
        }
    }

    pub fn set_local(&mut self, key: Symbol, value: Value<'a>) {
        self.locals.insert(key, value);
    }

    pub fn get_local(&self, key: Symbol) -> Option<&Value<'a>> {
        self.locals.get(&key)
    }

    pub fn get_global(&self, key: Symbol) -> Option<&Value<'a>> {
        self.globals.get(&key)
    }

    pub fn get_variable(&self, name: &VariableName) -> Option<&Value<'a>> {
        match name {
            VariableName::User(key) => self.locals.get(key),
            VariableName::Global(key) => self.globals.get(key),
            VariableName::Pseudo(key) => panic!(
                "expected pseudo-variable '{}' to have been expanded by caller",
                self.interner.resolve(key.into_inner())
            ),
        }
    }

    /// Returns the index corresponding to the start of the searchable buffer (current block)
    #[inline(always)]
    pub fn start(&self) -> usize {
        self.block.start
    }

    /// Returns the index corresponding to the end of the searchable buffer (current block)
    #[inline(always)]
    pub fn end(&self) -> usize {
        self.block.end
    }

    /// Returns the index (exclusive) corresponding to the end of the input buffer
    ///
    /// The returned index is valid for use in a range, but cannot be used to index the buffer.
    #[inline(always)]
    pub fn end_of_file(&self) -> usize {
        self.buffer.len()
    }

    /// Return the index corresponding to the next newline in the input
    ///
    /// This will be equal to `self.end_of_file()` if there are no more newlines in the input.
    #[inline(always)]
    pub fn end_of_line(&self) -> usize {
        self.eol
    }

    /// Return the index corresponding to the first byte following the next newline in the input
    ///
    /// This will be equal to `self.end_of_file()` if there are no more newlines in the input.
    #[inline(always)]
    pub fn start_of_next_line(&self) -> usize {
        if self.crlf {
            core::cmp::min(self.eol + 2, self.buffer.len())
        } else {
            core::cmp::min(self.eol + 1, self.buffer.len())
        }
    }

    /// Get an [Input] that can be used to search the entire underlying buffer
    pub fn search(&self) -> Input<'a> {
        Input::new(self.buffer, self.crlf)
    }

    /// Get an [Input] that can be used to search an arbitrary range of the underlying buffer
    pub fn search_range<R: RangeBounds<usize>>(&self, range: R) -> Input<'a> {
        Input::new(self.buffer, self.crlf).span(range)
    }

    /// Get an [Input] that can be used to search the unvisited portion of the current block
    pub fn search_block(&self) -> Input<'a> {
        Input::new(self.buffer, self.crlf).span(self.block)
    }

    /// Get an [Input] that can be used to search the unvisited portion of the current line
    pub fn search_line(&self) -> Input<'a> {
        Input::new(self.buffer, self.crlf).span(self.block.start..self.eol)
    }

    /// Consume `num_bytes` of the current block, ensuring all future searches of the
    /// block start after the consumed region.
    ///
    /// NOTE: This function will panic if `num_bytes` exceeds the size of the current block
    pub fn consume(&mut self, num_bytes: usize) {
        assert!(num_bytes < self.block.len());

        self.block.shrink_front(num_bytes);
        if self.block.start >= self.eol {
            let eof = self.buffer.len();
            self.eol = if self.crlf {
                text::find_next_crlf_or_eof(self.buffer, Range::new(self.block.start, eof))
                    .unwrap_or(eof)
            } else {
                text::find_next_lf_or_eof(self.buffer, Range::new(self.block.start, eof))
                    .unwrap_or(eof)
            };
        }
    }

    pub fn consume_newline(&mut self) {
        debug_assert!(
            self.is_end_of_line(self.block.start),
            "expected newline (crlf={}) at offset {}, invalid sequence starts with character code {}",
            self.crlf,
            self.block.start,
            self.buffer[self.block.start]
        );
        if self.crlf {
            self.consume(2);
        } else {
            self.consume(1);
        }
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
        if self.crlf {
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
        if self.crlf {
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
