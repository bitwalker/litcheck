mod all;
mod always;
mod any;
pub(crate) mod regex;
mod regex_set;
mod simple;
mod smart;
mod substring;
mod substring_set;
mod whitespace;

pub use self::all::MatchAll;
pub use self::always::AlwaysMatch;
pub use self::any::MatchAny;
pub use self::regex::RegexMatcher;
pub use self::regex_set::{RegexSetMatcher, RegexSetSearcher};
pub use self::simple::SimpleMatcher;
pub use self::smart::SmartMatcher;
pub use self::substring::SubstringMatcher;
pub use self::substring_set::{SubstringSetBuilder, SubstringSetMatcher};
pub use self::whitespace::AsciiWhitespaceMatcher;
