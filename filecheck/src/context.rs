use crate::common::*;

pub trait Context<'input, 'context> {
    fn config(&self) -> &'context Config;

    fn env(&self) -> &dyn LexicalScope<Value = Value<'input>>;

    fn env_mut<'env, 'this: 'env>(
        &'this mut self,
    ) -> &'env mut dyn LexicalScopeMut<Value = Value<'input>>;

    fn match_file(&self) -> ArcSource;

    fn match_file_bytes(&self) -> &[u8];

    fn input_file(&self) -> ArcSource;

    fn buffer(&self) -> &'input [u8];

    fn cursor(&self) -> &Cursor<'input>;

    fn cursor_mut(&mut self) -> &mut Cursor<'input>;

    fn move_to(&mut self, pos: CursorPosition) {
        self.cursor_mut().move_to(pos);
    }

    fn reset(&mut self) {
        self.cursor_mut().reset();
    }

    fn symbolize(&mut self, value: &str) -> Symbol;

    fn resolve(&mut self, value: Symbol) -> &str;

    /// Compute the value of `@LINE` for a given offset in the match file
    fn pseudo_line_for_offset(&self, offset: usize) -> usize {
        let bytes = self.match_file_bytes();
        let end = core::cmp::min(bytes.len(), offset);
        memchr::memchr_iter(b'\n', &bytes[..end]).count() + 1
    }

    /// Get an [Input] that can be used to search the entire underlying buffer
    fn search(&self) -> Input<'input> {
        self.cursor().search()
    }

    /// Get an [Input] that can be used to search from the current position to the
    /// end of the underlying buffer, ignoring the end bound of the cursor.
    fn search_to_end(&self) -> Input<'input> {
        self.cursor().search_to_end()
    }

    /// Get an [Input] that can be used to search the unvisited portion of the current block
    fn search_block(&self) -> Input<'input> {
        self.cursor().search_block()
    }

    /// Get an [Input] that can be used to search the unvisited portion of the current line
    fn search_line(&self) -> Input<'input> {
        self.cursor().search_line()
    }

    fn protect<'guard, 'this: 'guard>(&'this mut self) -> ContextGuard<'guard, 'input, 'context>;
}

pub trait ContextExt<'input, 'context>: Context<'input, 'context> {
    fn get_or_intern<S: AsRef<str>>(&mut self, value: S) -> Symbol;

    /// Get an [Input] that can be used to search an arbitrary range of the underlying buffer
    fn search_range<R>(&self, range: R) -> Input<'input>
    where
        R: RangeBounds<usize>,
    {
        self.cursor().search_range(range)
    }
}
impl<'input, 'context, C: Context<'input, 'context> + ?Sized> ContextExt<'input, 'context> for C {
    #[inline(always)]
    fn get_or_intern<S: AsRef<str>>(&mut self, value: S) -> Symbol {
        <C as Context<'input, 'context>>::symbolize(self, value.as_ref())
    }

    #[inline(always)]
    fn search_range<R>(&self, range: R) -> Input<'input>
    where
        R: RangeBounds<usize>,
    {
        self.cursor().search_range(range)
    }
}

pub struct ContextGuard<'guard, 'input, 'context> {
    config: &'context Config,
    match_file: ArcSource,
    input_file: ArcSource,
    scope: ScopeGuard<'guard, 'input, Value<'input>>,
    cursor: CursorGuard<'guard, 'input>,
    _marker: core::marker::PhantomData<&'input ()>,
}
impl<'guard, 'input, 'context> ContextGuard<'guard, 'input, 'context> {
    pub fn save(self) {
        self.cursor.save();
        self.scope.save();
    }

    pub fn extend_locals<I>(&mut self, bindings: I)
    where
        I: IntoIterator<Item = (Symbol, Value<'input>)>,
    {
        self.scope.extend(bindings);
    }
}
impl<'guard, 'input: 'guard, 'context> Context<'input, 'context>
    for ContextGuard<'guard, 'input, 'context>
{
    fn protect<'nested, 'this: 'nested>(
        &'this mut self,
    ) -> ContextGuard<'nested, 'input, 'context> {
        ContextGuard {
            config: self.config,
            match_file: self.match_file.clone(),
            input_file: self.match_file.clone(),
            scope: self.scope.protect(),
            cursor: self.cursor.protect(),
            _marker: core::marker::PhantomData,
        }
    }

    #[inline(always)]
    fn config(&self) -> &'context Config {
        self.config
    }

    #[inline(always)]
    fn env(&self) -> &dyn LexicalScope<Value = Value<'input>> {
        &self.scope
    }

    #[inline(always)]
    fn env_mut<'env, 'this: 'env>(
        &'this mut self,
    ) -> &'env mut dyn LexicalScopeMut<Value = Value<'input>> {
        &mut self.scope
    }

    #[inline]
    fn match_file(&self) -> ArcSource {
        self.match_file.clone()
    }

    #[inline(always)]
    fn match_file_bytes(&self) -> &[u8] {
        self.match_file.source_bytes()
    }

    #[inline]
    fn input_file(&self) -> ArcSource {
        self.input_file.clone()
    }

    #[inline(always)]
    fn buffer(&self) -> &'input [u8] {
        self.cursor.buffer()
    }

    #[inline(always)]
    fn cursor(&self) -> &Cursor<'input> {
        self.cursor.as_ref()
    }

    #[inline(always)]
    fn cursor_mut(&mut self) -> &mut Cursor<'input> {
        self.cursor.as_mut()
    }

    #[inline]
    fn symbolize(&mut self, value: &str) -> Symbol {
        self.scope.symbolize(value)
    }

    #[inline]
    fn resolve(&mut self, value: Symbol) -> &str {
        self.scope.resolve(value)
    }
}

pub struct MatchContext<'input, 'context> {
    /// The current global configuration
    pub config: &'context Config,
    pub env: Env<'input, 'context>,
    pub match_file: ArcSource,
    pub input_file: ArcSource,
    cursor: Cursor<'input>,
}
impl<'input, 'context: 'input> MatchContext<'input, 'context> {
    pub fn new(
        config: &'context Config,
        interner: &'context mut StringInterner,
        match_file: ArcSource,
        input_file: ArcSource,
        buffer: &'input [u8],
    ) -> Self {
        let env = Env::<'input, 'context>::from_config(config, interner);
        Self {
            config,
            env,
            match_file,
            input_file,
            cursor: Cursor::new(buffer),
        }
    }

    pub fn enter_block<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        self.cursor.set_bounds(range);
        if self.config.enable_var_scope {
            self.env.clear();
        }
    }
}
impl<'input, 'context: 'input> Context<'input, 'context> for MatchContext<'input, 'context> {
    fn protect<'guard, 'this: 'guard>(&'this mut self) -> ContextGuard<'guard, 'input, 'context> {
        ContextGuard {
            config: self.config,
            match_file: self.match_file.clone(),
            input_file: self.match_file.clone(),
            scope: self.env.protect(),
            cursor: self.cursor.protect(),
            _marker: core::marker::PhantomData,
        }
    }

    #[inline(always)]
    fn config(&self) -> &'context Config {
        self.config
    }

    #[inline(always)]
    fn env(&self) -> &dyn LexicalScope<Value = Value<'input>> {
        &self.env
    }

    #[inline(always)]
    fn env_mut<'env, 'this: 'env>(
        &'this mut self,
    ) -> &'env mut dyn LexicalScopeMut<Value = Value<'input>> {
        &mut self.env
    }

    #[inline]
    fn match_file(&self) -> ArcSource {
        self.match_file.clone()
    }

    #[inline(always)]
    fn match_file_bytes(&self) -> &[u8] {
        self.match_file.source_bytes()
    }

    #[inline]
    fn input_file(&self) -> ArcSource {
        self.input_file.clone()
    }

    #[inline(always)]
    fn buffer(&self) -> &'input [u8] {
        self.cursor.buffer
    }

    #[inline(always)]
    fn cursor(&self) -> &Cursor<'input> {
        &self.cursor
    }

    #[inline(always)]
    fn cursor_mut(&mut self) -> &mut Cursor<'input> {
        &mut self.cursor
    }

    #[inline]
    fn symbolize(&mut self, value: &str) -> Symbol {
        self.env.symbolize(value)
    }

    #[inline]
    fn resolve(&mut self, value: Symbol) -> &str {
        self.env.resolve(value)
    }
}
