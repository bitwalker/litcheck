use std::path::Path;

/// Searches `dir` for files which match `predicate`.
///
/// If `recursive` is false, the search only looks in `dir` and no deeper.
/// If `recursive` is true, the search will always recurse into child directories.
///
/// NOTE: This search does not follow symlinks.
pub fn search_directory<P, F>(
    dir: P,
    recursive: bool,
    mut predicate: F,
) -> impl Iterator<Item = Result<walkdir::DirEntry, walkdir::Error>>
where
    P: AsRef<Path>,
    F: Fn(&walkdir::DirEntry) -> bool,
{
    Searcher::new(dir.as_ref(), recursive, move |e| {
        apply_search_filter(e, &mut predicate)
    })
}

pub struct Searcher<P> {
    walker: walkdir::FilterEntry<walkdir::IntoIter, P>,
}
impl<P> Searcher<P>
where
    P: FnMut(&walkdir::DirEntry) -> bool,
{
    pub fn new(path: &Path, recursive: bool, predicate: P) -> Self {
        use walkdir::WalkDir;

        let mut walker = WalkDir::new(path).follow_links(true);
        if !recursive {
            walker = walker.max_depth(1);
        }
        let walker = walker.into_iter().filter_entry(predicate);

        Self { walker }
    }

    #[inline(always)]
    pub fn into_walker(self) -> walkdir::FilterEntry<walkdir::IntoIter, P> {
        self.walker
    }
}

impl<P> Iterator for Searcher<P>
where
    P: FnMut(&walkdir::DirEntry) -> bool,
{
    type Item = Result<walkdir::DirEntry, walkdir::Error>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        #[allow(clippy::while_let_on_iterator)]
        while let Some(entry) = self.walker.next() {
            if let Ok(entry) = entry {
                if entry.file_type().is_dir() {
                    continue;
                }
                return Some(Ok(entry));
            } else {
                return Some(entry);
            }
        }

        None
    }
}

#[inline]
fn apply_search_filter<F>(entry: &walkdir::DirEntry, predicate: &mut F) -> bool
where
    F: FnMut(&walkdir::DirEntry) -> bool,
{
    let path = entry.path();
    if path.is_dir() {
        return true;
    }
    predicate(entry)
}
