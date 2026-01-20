use std::{borrow::Cow, fmt, path::Path, sync::Arc};

use crate::{StaticCow, diagnostics::SourceLanguage};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FileName {
    Stdin,
    Path(Arc<Path>),
    Virtual(StaticCow<str>),
}

impl FileName {
    pub fn from_static_str(name: &'static str) -> Self {
        Self::Virtual(Cow::Borrowed(name))
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Stdin => "stdin",
            Self::Path(path) => path.to_str().unwrap_or("<invalid>"),
            Self::Virtual(name) => name.as_ref(),
        }
    }

    pub fn language(&self) -> SourceLanguage {
        let extension = match self {
            Self::Path(path) => path.extension().and_then(|ext| ext.to_str()).unwrap_or(""),
            Self::Stdin | Self::Virtual(_) => "",
        };
        SourceLanguage::from_extension(extension)
    }
}

impl PartialOrd for FileName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FileName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl From<&Path> for FileName {
    fn from(path: &Path) -> Self {
        if path.as_os_str() == "-" {
            Self::Stdin
        } else {
            Self::Path(path.to_path_buf().into_boxed_path().into())
        }
    }
}

impl From<Box<Path>> for FileName {
    fn from(path: Box<Path>) -> Self {
        if path.as_os_str() == "-" {
            Self::Stdin
        } else {
            Self::Path(path.into())
        }
    }
}

impl From<std::path::PathBuf> for FileName {
    fn from(path: std::path::PathBuf) -> Self {
        path.into_boxed_path().into()
    }
}

impl From<&str> for FileName {
    fn from(name: &str) -> Self {
        if name == "-" {
            Self::Stdin
        } else {
            Self::Virtual(Cow::Owned(name.to_string()))
        }
    }
}

impl From<String> for FileName {
    fn from(name: String) -> Self {
        if name == "-" {
            Self::Stdin
        } else {
            Self::Virtual(Cow::Owned(name))
        }
    }
}

impl fmt::Display for FileName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Stdin => f.write_str("stdin"),
            Self::Path(path) => write!(f, "{}", path.display()),
            Self::Virtual(name) => f.write_str(name),
        }
    }
}
