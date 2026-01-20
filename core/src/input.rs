use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

use crate::diagnostics::{ArcSource, FileName, Source};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Input(PathBuf);
impl From<std::ffi::OsString> for Input {
    fn from(s: std::ffi::OsString) -> Self {
        Self(PathBuf::from(s))
    }
}
impl From<&std::ffi::OsStr> for Input {
    fn from(s: &std::ffi::OsStr) -> Self {
        Self(PathBuf::from(s))
    }
}
impl From<&Path> for Input {
    fn from(path: &Path) -> Self {
        Self(path.to_path_buf())
    }
}
impl From<PathBuf> for Input {
    fn from(path: PathBuf) -> Self {
        Self(path)
    }
}
impl Input {
    pub fn exists(&self) -> bool {
        self.0.exists()
    }

    pub fn is_file(&self) -> bool {
        self.0.is_file()
    }

    pub fn is_directory(&self) -> bool {
        self.0.is_dir()
    }

    pub fn path(&self) -> &Path {
        self.0.as_ref()
    }

    pub fn filename(&self) -> FileName {
        if self.0.as_os_str() == "-" {
            FileName::Stdin
        } else {
            FileName::Path(self.0.clone().into_boxed_path())
        }
    }

    pub fn into_arc_source(&self, strict: bool) -> std::io::Result<ArcSource> {
        self.into_source(strict).map(ArcSource::new)
    }

    pub fn into_source(&self, strict: bool) -> std::io::Result<Source<'static>> {
        let name = self.filename();
        let code = self.read_to_string(strict).map(Cow::Owned)?;
        log::trace!(target: "input", "read '{name}': '{code}'");
        Ok(Source { name, code })
    }

    pub fn get_file_types(&self, file_types: &[String]) -> Result<Vec<Input>, walkdir::Error> {
        use walkdir::WalkDir;

        let mut inputs = vec![];
        let walker = WalkDir::new(&self.0).into_iter();
        for entry in walker.filter_entry(|e| is_matching_file_type_or_dir(e, file_types)) {
            let input = Self(entry?.into_path());
            if input.is_directory() {
                let mut children = input.get_file_types(file_types)?;
                inputs.append(&mut children);
            } else {
                inputs.push(input);
            }
        }

        Ok(inputs)
    }

    pub fn glob(&self, pattern: &str) -> Result<Vec<Input>, walkdir::Error> {
        use glob::Pattern;
        assert!(
            self.is_directory(),
            "cannot call `glob` on a non-directory path: {}",
            self.0.display()
        );

        // We need to create a pattern that extends the current path, while treating
        // the path as a literal match. We do that by first converting the path to a string,
        // escaping the string of any pattern meta characters, then joining the provided
        // glob pattern to the path so that it matches anything underneath the directory
        // represented by `self`
        let path = self.0.as_os_str().to_string_lossy();
        let mut pat = Pattern::escape(&path);
        pat.push_str(std::path::MAIN_SEPARATOR_STR);
        pat.push_str(pattern);
        let pattern = Pattern::new(&pat).expect("invalid glob pattern");
        self.glob_pattern(&pattern)
    }

    fn glob_pattern(&self, pattern: &glob::Pattern) -> Result<Vec<Input>, walkdir::Error> {
        use walkdir::WalkDir;

        let mut inputs = vec![];
        let walker = WalkDir::new(&self.0).into_iter();
        for entry in walker.filter_entry(|e| is_dir_or_pattern_match(e, pattern)) {
            let input = Self(entry?.into_path());
            if input.is_directory() {
                let mut children = input.glob_pattern(pattern)?;
                inputs.append(&mut children);
            } else {
                inputs.push(input);
            }
        }

        Ok(inputs)
    }

    pub fn open(&self) -> std::io::Result<impl std::io::BufRead> {
        use either::Either;
        use std::fs::File;

        Ok(if self.0.as_os_str() == "-" {
            Either::Left(std::io::stdin().lock())
        } else {
            let file = if self.0.is_absolute() {
                File::open(&self.0)?
            } else {
                let path = self.0.canonicalize()?;
                File::open(path)?
            };
            Either::Right(std::io::BufReader::new(file))
        })
    }

    pub fn read_to_string(&self, strict: bool) -> std::io::Result<String> {
        use std::io::{BufRead, Read};

        let mut buf = self.open()?;
        let mut content = String::with_capacity(1024);
        if strict {
            buf.read_to_string(&mut content)?;
            Ok(content)
        } else {
            // Normalize line endings via `lines`
            for (i, line) in buf.lines().enumerate() {
                let mut line = line?;
                // SAFETY: We are able to guarantee that we do not violate
                // the utf-8 property of `content` here, because both lines
                // are known to be valid utf-8, and we're joining them with
                // another valid utf-8 character, as '\n' has the same
                // representation in ascii and utf-8
                unsafe {
                    let bytes = content.as_mut_vec();
                    if i > 0 {
                        bytes.push(b'\n');
                    }
                    bytes.append(line.as_mut_vec());
                }
            }

            Ok(content)
        }
    }
}

fn is_matching_file_type_or_dir(entry: &walkdir::DirEntry, file_types: &[String]) -> bool {
    let path = entry.path();
    if path.is_dir() {
        return true;
    }
    if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
        file_types.iter().any(|ft| ft == ext)
    } else {
        false
    }
}

fn is_dir_or_pattern_match(entry: &walkdir::DirEntry, pattern: &glob::Pattern) -> bool {
    let path = entry.path();
    if path.is_dir() {
        return true;
    }
    pattern.matches_path(path)
}
