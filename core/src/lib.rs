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
pub mod reporting;
pub mod symbol;
pub mod text;
pub mod variables;

pub use self::{
    input::Input,
    symbol::{Symbol, symbols},
};

pub type StaticCow<T> = std::borrow::Cow<'static, T>;
