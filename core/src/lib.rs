/// This is an implementation of `std::assert_matches::assert_matches`
/// so it can be removed when that feature stabilizes upstream
#[macro_export]
macro_rules! assert_matches {
    ($left:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        match $left {
            $( $pattern )|+ $( if $guard )? => {}
            ref left_val => {
                panic!(r#"
assertion failed: `(left matches right)`
    left: `{:?}`,
    right: `{}`"#, left_val, stringify!($($pattern)|+ $(if $guard)?));
            }
        }
    };

    ($left:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )?, $msg:literal $(,)?) => {
        match $left {
            $( $pattern )|+ $( if $guard )? => {}
            ref left_val => {
                panic!(concat!(r#"
assertion failed: `(left matches right)`
    left: `{:?}`,
    right: `{}`
"#, $msg), left_val, stringify!($($pattern)|+ $(if $guard)?));
            }
        }
    };

    ($left:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )?, $msg:literal, $($arg:tt)+) => {
        match $left {
            $( $pattern )|+ $( if $guard )? => {}
            ref left_val => {
                panic!(concat!(r#"
assertion failed: `(left matches right)`
    left: `{:?}`,
    right: `{}`
"#, $msg), left_val, stringify!($($pattern)|+ $(if $guard)?), $($arg)+);
            }
        }
    }
}

pub mod diagnostics;
pub mod fs;
mod input;
pub mod range;
pub mod text;
pub mod variables;

pub use self::input::Input;

pub type StaticCow<T> = std::borrow::Cow<'static, T>;

pub type Symbol = string_interner::DefaultSymbol;

pub struct StringInterner(
    string_interner::StringInterner<
        string_interner::backend::StringBackend<Symbol>,
        core::hash::BuildHasherDefault<rustc_hash::FxHasher>,
    >,
);
impl Default for StringInterner {
    fn default() -> Self {
        Self(string_interner::StringInterner::new())
    }
}
impl StringInterner {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn get_or_intern(&mut self, s: impl AsRef<str>) -> Symbol {
        self.0.get_or_intern(s)
    }

    #[track_caller]
    pub fn resolve(&self, symbol: Symbol) -> &str {
        self.0.resolve(symbol).expect("invalid symbol")
    }
}
