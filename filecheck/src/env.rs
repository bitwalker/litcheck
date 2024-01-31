use std::{collections::BTreeMap, rc::Rc};

use crate::common::*;

/// A [LexicalScope] represents a scoped environment used for
/// expression evaluation during pattern matching in CHECK rules.
///
/// Scopes can be nested arbitrarily deep, and always provide
/// explicit access to globally-defined variables (which are in
/// a separate namespace from locally-bound variables). Local
/// bindings of a given scope always take precedence over those
/// of an outer scope, but bindings missing from the current scope
/// can be provided by parent scopes.
///
/// This trait defines read-only operations, see [LexicalScopeMut]
/// for the set of mutable operations that can be performed.
pub trait LexicalScope {
    /// The value type bound by variables
    type Value;

    /// Get the value bound to `name`
    fn get(&self, name: &VariableName) -> Option<&Self::Value> {
        match name {
            VariableName::User(key) => self.get_local(key),
            VariableName::Global(key) => self.get_global(key),
            VariableName::Pseudo(_) => {
                panic!("expected pseudo-variable to have been expanded by caller")
            }
        }
    }

    /// Get the value locally bound to `symbol`
    fn get_local(&self, symbol: &Symbol) -> Option<&Self::Value>;

    /// Get the value globally bound to `symbol`
    fn get_global(&self, symbol: &Symbol) -> Option<&Self::Value>;

    /// Resolve an interned [Symbol] to its string value
    fn resolve(&self, symbol: Symbol) -> &str;
}
impl<S> LexicalScope for &S
where
    S: LexicalScope + ?Sized,
{
    type Value = <S as LexicalScope>::Value;

    #[inline(always)]
    fn get(&self, name: &VariableName) -> Option<&Self::Value> {
        (**self).get(name)
    }

    #[inline(always)]
    fn get_local(&self, symbol: &Symbol) -> Option<&Self::Value> {
        (**self).get_local(symbol)
    }

    #[inline(always)]
    fn get_global(&self, symbol: &Symbol) -> Option<&Self::Value> {
        (**self).get_global(symbol)
    }

    #[inline(always)]
    fn resolve(&self, symbol: Symbol) -> &str {
        (**self).resolve(symbol)
    }
}
impl<'a, S> LexicalScope for &'a mut S
where
    S: LexicalScope + ?Sized,
{
    type Value = <S as LexicalScope>::Value;

    #[inline(always)]
    fn get(&self, name: &VariableName) -> Option<&Self::Value> {
        (**self).get(name)
    }

    #[inline(always)]
    fn get_local(&self, symbol: &Symbol) -> Option<&Self::Value> {
        (**self).get_local(symbol)
    }

    #[inline(always)]
    fn get_global(&self, symbol: &Symbol) -> Option<&Self::Value> {
        (**self).get_global(symbol)
    }

    #[inline(always)]
    fn resolve(&self, symbol: Symbol) -> &str {
        (**self).resolve(symbol)
    }
}

/// This trait provides the mutable operations of a [LexicalScope]
pub trait LexicalScopeMut: LexicalScope {
    /// Get the underling [StringInterner] used by this scope
    fn interner(&mut self) -> &mut StringInterner;

    /// Get a [Symbol] representing the interned string `value`
    fn symbolize(&mut self, value: &str) -> Symbol;

    /// Insert a new local binding
    fn insert(&mut self, symbol: Symbol, value: Self::Value);

    /// Take the local bindings of this scope, emptying the scope
    fn take(&mut self) -> BTreeMap<Symbol, Self::Value>;

    /// Clear the local bindings of this scope
    fn clear(&mut self);
}
impl<'a, S> LexicalScopeMut for &'a mut S
where
    S: LexicalScopeMut + ?Sized,
{
    #[inline(always)]
    fn interner(&mut self) -> &mut StringInterner {
        (**self).interner()
    }

    #[inline(always)]
    fn symbolize(&mut self, value: &str) -> Symbol {
        (**self).symbolize(value)
    }

    #[inline(always)]
    fn insert(&mut self, symbol: Symbol, value: Self::Value) {
        (**self).insert(symbol, value)
    }

    #[inline(always)]
    fn take(&mut self) -> BTreeMap<Symbol, Self::Value> {
        (**self).take()
    }

    #[inline(always)]
    fn clear(&mut self) {
        (**self).clear();
    }
}
impl<V> dyn LexicalScopeMut<Value = V> {
    #[inline(always)]
    pub fn get_or_intern<S>(&mut self, value: S) -> Symbol
    where
        S: AsRef<str>,
    {
        self.symbolize(value.as_ref())
    }

    pub fn extend<I>(&mut self, bindings: I)
    where
        I: IntoIterator<Item = (Symbol, <Self as LexicalScope>::Value)>,
    {
        bindings.into_iter().for_each(move |(k, v)| {
            self.insert(k, v);
        });
    }
}

/// This trait is separate from [LexicalScopeMut] to ensure that [LexicalScopeMut]
/// can be used as a trait object. This trait is automatically implemented for all
/// implementations of [LexicalScopeMut].
pub trait LexicalScopeExtend {
    type Value;

    /// Extend the local set of bindings from the given iterator.
    fn extend<I>(&mut self, bindings: I)
    where
        I: IntoIterator<Item = (Symbol, Self::Value)>;
}
impl<'a, S> LexicalScopeExtend for &'a mut S
where
    S: LexicalScopeExtend + ?Sized,
{
    type Value = <S as LexicalScopeExtend>::Value;

    #[inline]
    fn extend<I>(&mut self, bindings: I)
    where
        I: IntoIterator<Item = (Symbol, Self::Value)>,
    {
        (**self).extend(bindings);
    }
}

/// This struct represents the top-level environment used by expressions
/// evaluated as part of a CHECK pattern. Currently, this environment
/// consists solely of local and global variable bindings, along with the
/// string interner state used for interning variable names.
pub struct Env<'input, 'context> {
    /// The current string interner
    interner: &'context mut StringInterner,
    /// Global variables set on command line
    globals: Rc<BTreeMap<Symbol, Value<'input>>>,
    /// Local variables bound during matching
    locals: BTreeMap<Symbol, Value<'input>>,
}
impl<'input, 'context: 'input> Env<'input, 'context> {
    /// Create a new environment with the given set of predefined global variables
    ///
    /// The local binding set will be empty.
    pub fn new(
        interner: &'context mut StringInterner,
        globals: Rc<BTreeMap<Symbol, Value<'input>>>,
    ) -> Self {
        Self {
            interner,
            globals,
            locals: Default::default(),
        }
    }

    /// Create a new environment, initialized from the given configuration
    pub fn from_config<'i, 'ctx: 'i>(
        config: &'ctx Config,
        interner: &'ctx mut StringInterner,
    ) -> Env<'i, 'ctx> {
        let globals = Rc::new(BTreeMap::<_, Value<'i>>::from_iter(
            config.variables.iter().map(|v| {
                (
                    interner.get_or_intern(v.name.as_ref()),
                    match v.value {
                        Value::Undef => Value::Undef,
                        Value::Str(ref s) => Value::Str(s.clone()),
                        Value::Num(ref n) => Value::Num(n.clone()),
                    },
                )
            }),
        ));
        Env {
            interner,
            globals,
            locals: Default::default(),
        }
    }

    /// Guard this scope with a nested scope
    ///
    /// A nested scope must explicitly save its bindings in order for
    /// them to be persisted back to its parent scope.
    pub fn protect<'guard, 'this: 'guard>(
        &'this mut self,
    ) -> ScopeGuard<'guard, 'input, Value<'input>> {
        let globals = self.globals.clone();
        ScopeGuard {
            interner: self.interner,
            parent: &mut self.locals,
            globals,
            locals: Default::default(),
            _marker: core::marker::PhantomData,
        }
    }
}
impl<'input, 'context: 'input> LexicalScope for Env<'input, 'context> {
    type Value = Value<'input>;

    fn get_local(&self, symbol: &Symbol) -> Option<&Self::Value> {
        self.locals.get(symbol)
    }

    fn get_global(&self, symbol: &Symbol) -> Option<&Self::Value> {
        self.globals.get(symbol)
    }

    fn resolve(&self, symbol: Symbol) -> &str {
        self.interner.resolve(symbol)
    }
}
impl<'input, 'context: 'input> LexicalScopeMut for Env<'input, 'context> {
    #[inline(always)]
    fn interner(&mut self) -> &mut StringInterner {
        self.interner
    }

    fn symbolize(&mut self, value: &str) -> Symbol {
        self.interner.get_or_intern(value)
    }

    fn insert(&mut self, symbol: Symbol, value: Self::Value) {
        self.locals.insert(symbol, value);
    }

    fn take(&mut self) -> BTreeMap<Symbol, Self::Value> {
        core::mem::take(&mut self.locals)
    }

    fn clear(&mut self) {
        self.locals.clear();
    }
}
impl<'input, 'context: 'input> LexicalScopeExtend for Env<'input, 'context> {
    type Value = Value<'input>;

    fn extend<I>(&mut self, bindings: I)
    where
        I: IntoIterator<Item = (Symbol, Self::Value)>,
    {
        self.locals.extend(bindings);
    }
}

/// This struct is used to introduce a new scope which "protects" the
/// scope from which it is derived.
///
/// It can be treated the same as an [Env], except any changes made
/// through a [ScopeGuard] will be dropped when the guard is dropped,
/// unless they are explicitly persisted to the parent scope using
/// [ScopeGuard::save].
///
/// This is used to optimistically evaluate patterns which may bind
/// new variables in the course of matching the input, but fail at
/// some later point. In such cases, the guard can be dropped, and
/// all of the changes made up to that point will be ignored. When
/// a rule succeeds, the scope can be persisted explicitly to the
/// outer scope.
pub struct ScopeGuard<'a, 'input: 'a, V: 'input + 'a> {
    interner: &'a mut StringInterner,
    parent: &'a mut BTreeMap<Symbol, V>,
    globals: Rc<BTreeMap<Symbol, V>>,
    locals: BTreeMap<Symbol, V>,
    _marker: core::marker::PhantomData<&'input ()>,
}
impl<'a, 'input> ScopeGuard<'a, 'input, Value<'input>> {
    /// Consume this [ScopeGuard] and persist any changes to the
    /// parent [LexicalScope].
    pub fn save(mut self) {
        let locals = core::mem::take(&mut self.locals);
        self.parent.extend(locals);
    }

    pub fn protect<'guard, 'this: 'guard>(
        &'this mut self,
    ) -> ScopeGuard<'guard, 'input, Value<'input>> {
        let globals = self.globals.clone();
        ScopeGuard {
            interner: self.interner,
            parent: &mut self.locals,
            globals,
            locals: Default::default(),
            _marker: core::marker::PhantomData,
        }
    }
}
impl<'a, 'input: 'a, V: 'input> LexicalScope for ScopeGuard<'a, 'input, V> {
    type Value = V;

    fn get_local(&self, symbol: &Symbol) -> Option<&Self::Value> {
        self.locals.get(symbol).or_else(|| self.parent.get(symbol))
    }

    #[inline(always)]
    fn get_global(&self, symbol: &Symbol) -> Option<&Self::Value> {
        self.globals.get(symbol)
    }

    #[inline(always)]
    fn resolve(&self, symbol: Symbol) -> &str {
        self.interner.resolve(symbol)
    }
}
impl<'a, 'input: 'a, V: 'input> LexicalScopeMut for ScopeGuard<'a, 'input, V> {
    #[inline(always)]
    fn interner(&mut self) -> &mut StringInterner {
        self.interner
    }

    #[inline(always)]
    fn symbolize(&mut self, value: &str) -> Symbol {
        self.interner.get_or_intern(value)
    }

    fn insert(&mut self, symbol: Symbol, value: Self::Value) {
        self.locals.insert(symbol, value);
    }

    #[inline]
    fn take(&mut self) -> BTreeMap<Symbol, Self::Value> {
        core::mem::take(&mut self.locals)
    }

    fn clear(&mut self) {
        self.locals.clear();
    }
}
impl<'a, 'input: 'a, V: 'input> LexicalScopeExtend for ScopeGuard<'a, 'input, V> {
    type Value = V;

    fn extend<I>(&mut self, bindings: I)
    where
        I: IntoIterator<Item = (Symbol, Self::Value)>,
    {
        self.locals.extend(bindings);
    }
}
