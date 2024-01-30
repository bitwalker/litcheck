use std::{
    borrow::Cow,
    convert::AsRef,
    path::{Path, PathBuf},
};

use either::Either::{self, Left, Right};

/// Represents an entry in a [PathPrefixTree]
pub struct Entry<'a, V> {
    pub path: PathBuf,
    pub data: &'a V,
}
impl<'a, V> Entry<'a, V> {
    #[inline(always)]
    pub fn into_path(self) -> PathBuf {
        self.path
    }

    #[inline(always)]
    pub fn into_value(self) -> &'a V {
        self.data
    }
}
impl<'a, V> AsRef<Path> for Entry<'a, V> {
    #[inline(always)]
    fn as_ref(&self) -> &Path {
        self.path.as_path()
    }
}
impl<'a, V> AsRef<V> for Entry<'a, V> {
    #[inline(always)]
    fn as_ref(&self) -> &V {
        self.data
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TryInsertError {
    #[error("unable to insert path due to existing entry with conflicting file type")]
    PathTypeConflict,
    #[error("the path '{}' is unreachable: an ancestor of this path is a file", .0.display())]
    Unreachable(PathBuf),
}

/// A [PathPrefixTree] is a tree data structure optimized for indexing/searching
/// a directory hierarchy in various ways. In particular, it is efficient to ask:
///
/// * If a path is contained in the tree
/// * The nearest ancestor contained in the tree for a given path
///
/// Furthermore, the structure of the tree is very compact, only requiring branching
/// where multiple paths with the same prefix diverge. As a result, the depth of the
/// tree is typically much less than the depth of the corresponding directory hierarchy,
/// and is always bounded by the depth of the directory structure itself.
///
/// This data structure is lexicographically ordered, and can be reified as a flat set
/// of paths, or traversed like a tree.
pub struct PathPrefixTree<V> {
    tree: PrefixTree,
    /// Storage for the data associated with nodes in the tree
    data: Vec<V>,
}
impl<V> Default for PathPrefixTree<V> {
    fn default() -> Self {
        Self {
            tree: PrefixTree::default(),
            data: vec![],
        }
    }
}
impl<V: Clone> Clone for PathPrefixTree<V> {
    fn clone(&self) -> Self {
        let tree = self.tree.clone();
        let data = self.data.clone();
        Self { tree, data }
    }
}
impl<'a, V> FromIterator<&'a str> for PathPrefixTree<V>
where
    V: Default,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = &'a str>,
    {
        let mut tree = Self::default();
        for path in iter {
            tree.insert(path, V::default());
        }
        tree
    }
}
impl<'a, V> FromIterator<&'a Path> for PathPrefixTree<V>
where
    V: Default,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = &'a Path>,
    {
        let mut tree = Self::default();
        for path in iter {
            tree.insert(path, V::default());
        }
        tree
    }
}
impl<V, P> FromIterator<(P, V)> for PathPrefixTree<V>
where
    P: AsRef<Path>,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (P, V)>,
    {
        let mut tree = Self::default();
        for (path, value) in iter {
            tree.insert(path.as_ref(), value);
        }
        tree
    }
}
impl<V> PathPrefixTree<V> {
    /// Get a new, empty [PathPrefixTree]
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert `path` in this tree with the given value associated with the resulting node.
    ///
    /// If `path` is already contained in this tree (using the definition of containment
    /// as described in `contains`), then this operation will replace the value for that node.
    pub fn insert<P: AsRef<Path>>(&mut self, path: P, value: V) {
        self.try_insert(path, value).expect("insert failed")
    }

    /// Try to insert `path` in this tree with the given value associated with the resulting node.
    ///
    /// If `path` is already contained in this tree (using the definition of containment
    /// as described in `contains`), then this operation will replace the value for that node,
    /// unless the file type for that node is in conflict, in which case an error will be returned.
    pub fn try_insert<P: AsRef<Path>>(&mut self, path: P, value: V) -> Result<(), TryInsertError> {
        let data = DataKey(self.data.len());
        self.data.push(value);
        self.tree.insert(path.as_ref(), data)
    }

    /// Reset this tree to its default, empty state.
    pub fn clear(&mut self) {
        self.tree.clear();
        self.data.clear();
    }

    /// Returns true if `path` has been inserted in this tree.
    pub fn contains<P: AsRef<Path>>(&self, path: P) -> bool {
        self.tree.contains(path.as_ref())
    }

    /// Get a reference to the data associated with `path`, if present in the tree
    pub fn get<P: AsRef<Path>>(&self, path: P) -> Option<&V> {
        let key = self.tree.get(path.as_ref())?;
        Some(&self.data[key.as_usize()])
    }

    /// Get a mutable reference to the data associated with `path`, if present in the tree
    pub fn get_mut<P: AsRef<Path>>(&mut self, path: P) -> Option<&mut V> {
        let key = self.tree.get(path.as_ref())?;
        Some(&mut self.data[key.as_usize()])
    }

    /// Get the nearest entry in the tree which is an ancestor of `path`
    pub fn nearest_ancestor<P: AsRef<Path>>(&self, path: P) -> Option<Entry<'_, V>> {
        self.tree
            .nearest_ancestor(path.as_ref())
            .map(|(path, key)| Entry {
                path,
                data: &self.data[key.as_usize()],
            })
    }

    /// Iterate over all of the entries inserted in this tree.
    pub fn iter(&self) -> impl Iterator<Item = Entry<'_, V>> + '_ {
        Dfs::new(self, /*only_leaves=*/ false, None)
    }

    /// Iterate over all of the entries inserted in this tree,
    /// with a path that refers to a file.
    ///
    /// This is like `iter`, but does not emit entries for directories.
    pub fn files(&self) -> impl Iterator<Item = Entry<'_, V>> + '_ {
        Dfs::new(self, /*only_leaves=*/ true, None)
    }

    /// Iterate over all entries in the tree which are descendants of `path`.
    ///
    /// If `max_depth` is set, the search is restricted to entries with a path
    /// containing no more than `max_depth` additional path components than
    /// `path` itself.
    ///
    /// For example, with a tree containing `/foo/bar`, `/foo/bar/baz` and
    /// `/foo/bar/baz/qux`, calling this function with `/foo/bar` and a max depth
    /// of 2, will only return `/foo/bar/baz`, not `foo/bar/baz/qux`, since
    /// the latter is 2 levels deeper in the hierarchy.
    pub fn children<P: AsRef<Path>>(
        &self,
        path: P,
        max_depth: Option<usize>,
    ) -> impl Iterator<Item = Entry<'_, V>> + '_ {
        Dfs::at(self, path.as_ref(), /*only_leaves=*/ false, max_depth)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum PathType {
    /// The path does not exist, and we've observed no descendant paths yet
    Unknown,
    /// The path is known to exist, and is not a directory
    File,
    /// The path is either known to exist and is a directory, or was previously
    /// Unknown, but one or more descendant paths have been observed, which
    /// dictates that this path must be a directory.
    Dir,
}

/// This is the type-erased implementation backing [PathPrefixTree]
#[derive(Default, Clone)]
struct PrefixTree {
    /// We permit multiple top-level root nodes in this tree,
    /// making it actually a forest, but you could also think
    /// of this field as representing children of a theoretical
    /// "top" node that is the parent of all possible paths.
    ///
    /// On a Unix system, that would implicitly be `/`, but on
    /// Windows there can be multiple prefixes, e.g. `C:\`, etc.
    roots: Vec<Node>,
}
impl PrefixTree {
    /// Insert `path` into this tree with `data`
    pub fn insert(&mut self, path: &Path, data: DataKey) -> Result<(), TryInsertError> {
        let (path, ty) = canonicalize_and_type(path);

        // Try to find existing root to insert into
        if let Some(sort) = self.try_insert_existing(&path, ty, data)? {
            // Found root with common prefix
            if sort {
                self.roots.sort();
            }
            return Ok(());
        }

        self.insert_new(path.into_owned(), ty, data);

        Ok(())
    }

    fn try_insert_existing(
        &mut self,
        path: &Path,
        ty: PathType,
        data: DataKey,
    ) -> Result<Option<bool>, TryInsertError> {
        for root in self.roots.iter_mut() {
            if root.has_common_prefix(path) {
                // If we modified `root` due to this insertion, require `roots` to be re-sorted
                return insert_into_prefix(root, path, ty, data).map(|depth| Some(depth == 0));
            }
        }

        Ok(None)
    }

    fn insert_new(&mut self, mut path: PathBuf, ty: PathType, data: DataKey) {
        // Create new root node
        let root = match ty {
            PathType::Dir => Node::Branch {
                component: path,
                children: vec![],
                data: Some(data),
            },
            ty @ (PathType::Unknown | PathType::File) => {
                let mut components = path.components();
                let file = PathBuf::from(components.next_back().unwrap().as_os_str());
                let child = Node::Leaf {
                    component: file,
                    ty,
                    data: Some(data),
                };
                path.pop();
                Node::Branch {
                    component: path,
                    children: vec![child],
                    data: None,
                }
            }
        };

        // Insert the new root node, and ensure the roots are ordered
        self.roots.push(root);
        self.roots.sort();
    }

    /// Reset this tree to its default, empty state.
    pub fn clear(&mut self) {
        self.roots.clear();
    }

    pub fn contains(&self, path: &Path) -> bool {
        let (path, _) = canonicalize_and_type(path);
        self.roots.iter().any(|root| root.contains(&path))
    }

    pub fn get(&self, path: &Path) -> Option<DataKey> {
        let (path, _) = canonicalize_and_type(path);
        self.roots.iter().find_map(|root| {
            if root.has_common_prefix(&path) {
                root.get(&path).and_then(|n| n.data())
            } else {
                None
            }
        })
    }

    pub fn nearest_ancestor(&self, path: &Path) -> Option<(PathBuf, DataKey)> {
        let (path, _) = canonicalize_and_type(path);
        self.roots
            .iter()
            .find_map(|root| root.nearest_ancestor(&path))
    }
}

/// This is similar to `Path::canonicalize`, except it does a few things differently:
///
/// * If the given path cannot be canonicalized the "standard" way, we use a fallback
///   method as described in the following points.
/// * In the fallback canonicalization, the path type is always Unknown. If such a path
///   is inserted in the tree, it may become Dir if descendant paths are inserted in the
///   tree.
/// * As a result of the above this does not try to determine if a file path is treated as a
///   directory, e.g. `/foo/bar.txt/qux`. We assume that the user of the [PathPrefixTree] is
///   either validating paths themselves, or is ok with invalid paths.
/// * The fallback canonicalization only partially expands parent references (i.e. `..`). When
///   these occur at the start of the path, they are preserved, not expanded. When they occur
///   following some other type of path component, they cause that component to be dropped, and
///   the `..` is considered resolved (and also dropped).
fn canonicalize_and_type<'a>(path: &'a Path) -> (Cow<'a, Path>, PathType) {
    use smallvec::SmallVec;
    use std::path::Component;

    const PATH_SEPARATOR_SIZE: usize = std::path::MAIN_SEPARATOR_STR.as_bytes().len();

    if let Ok(path) = path.canonicalize() {
        let path: Cow<'a, Path> = Cow::Owned(path);
        let ty = match path.metadata() {
            Err(_) => PathType::Unknown,
            Ok(metadata) => {
                if metadata.is_dir() {
                    PathType::Dir
                } else {
                    PathType::File
                }
            }
        };
        return (path, ty);
    }

    let mut components = SmallVec::<[Component; 4]>::default();
    let mut canonical = true;
    let mut size_in_bytes = 0;
    for component in path.components() {
        match component {
            component @ (Component::Normal(_) | Component::RootDir | Component::Prefix(_)) => {
                size_in_bytes += component.as_os_str().len() + PATH_SEPARATOR_SIZE;
                components.push(component);
            }
            // This only occurs when `.` is the first path component, so drop this
            // component as we automatically assume relative paths are relative to
            // the current working directory
            Component::CurDir => {
                canonical = false;
            }
            // This occurs when `..` is found anywhere in the path. We normalize
            // these by dropping the preceding component when there is a preceding
            // component, and when it is not `..`. If there are no preceding
            // components, then `..` becomes the first component.
            Component::ParentDir => match components.last() {
                None | Some(Component::ParentDir) => {
                    let component = Component::ParentDir;
                    size_in_bytes += component.as_os_str().len() + PATH_SEPARATOR_SIZE;
                    components.push(component);
                }
                Some(_) => {
                    canonical = false;
                    let component = unsafe { components.pop().unwrap_unchecked() };
                    size_in_bytes -= component.as_os_str().len() + PATH_SEPARATOR_SIZE;
                }
            },
        }
    }

    // If `path` is already canonical, return it unmodified
    if canonical {
        return (Cow::Borrowed(path), PathType::Unknown);
    }

    // Otherwise, we need to construct the canonical path
    let mut path = PathBuf::with_capacity(size_in_bytes);
    for component in components.into_iter() {
        path.push(component);
    }
    (Cow::Owned(path), PathType::Unknown)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct DataKey(usize);
impl DataKey {
    #[inline(always)]
    const fn as_usize(self) -> usize {
        self.0
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Prefix {
    /// The node path exactly equals the input path
    Exact,
    /// The input path is an ancestor of the node path.
    Ancestor,
    /// The input path is a child of the node path
    Child,
    /// The input path and node path diverge, and the
    /// given path is the common prefix of the two.
    Partial(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Node {
    /// A leaf node is always a file
    Leaf {
        component: PathBuf,
        ty: PathType,
        data: Option<DataKey>,
    },
    /// A branch node is always a directory, and may have zero or more children
    Branch {
        component: PathBuf,
        children: Vec<Node>,
        data: Option<DataKey>,
    },
}
impl Ord for Node {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.component().cmp(other.component())
    }
}
impl PartialOrd for Node {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Node {
    /// Get the path component for this node
    #[inline]
    pub fn component(&self) -> &Path {
        match self {
            Self::Leaf { ref component, .. } | Self::Branch { ref component, .. } => {
                component.as_path()
            }
        }
    }

    /// Get the data key for this node, if applicable
    #[inline]
    pub fn data(&self) -> Option<DataKey> {
        match self {
            Self::Leaf { ref data, .. } | Self::Branch { ref data, .. } => *data,
        }
    }

    /// Returns the common prefix with `path`.
    ///
    /// If `None`, there is no common prefix.
    ///
    /// If `Some`, see the docs for [PrefixMatch] for details on what the various
    /// match types mean.
    pub fn common_prefix(&self, path: &Path) -> Option<Prefix> {
        assert_ne!(
            path,
            Path::new(""),
            "invalid empty path given to common_prefix"
        );

        let mut ac = self.component().components();
        let mut bc = path.components();
        let mut common_prefix = PathBuf::new();
        loop {
            match (ac.next(), bc.next()) {
                // We've reached the end of the components of `self`, so we've
                // got at least some common prefix with `path`, go ahead and return
                // it without further consideration.
                (None, None) => break Some(Prefix::Exact),
                (None, Some(_)) => break Some(Prefix::Child),
                // The next component in both paths is common, so append it to
                // our common prefix.
                (Some(a), Some(b)) if a == b => {
                    common_prefix.push(a);
                }
                // We diverge from `path` here.
                (Some(_), Some(_)) => {
                    if common_prefix == Path::new("") {
                        break None;
                    } else {
                        break Some(Prefix::Partial(common_prefix));
                    }
                }
                // We've reached the end of the components of `path` before `self`.
                // This indicates that `path` is shallower than `self`, and thus `self`
                // must be split to accomodate `path`.
                (Some(_), None) => {
                    break Some(Prefix::Ancestor);
                }
            }
        }
    }

    /// Returns true if `self` has a common prefix with `path`.
    pub fn has_common_prefix(&self, path: &Path) -> bool {
        assert_ne!(
            path,
            Path::new(""),
            "invalid empty path given to has_common_prefix"
        );

        let mut ac = self.component().components();
        let mut bc = path.components();
        let mut has_minimum_prefix = false;
        loop {
            match (ac.next(), bc.next()) {
                (None, _) => break true,
                (Some(a), Some(b)) if a == b => {
                    has_minimum_prefix = true;
                }
                (Some(_), Some(_)) => break has_minimum_prefix,
                (Some(_), None) => break true,
            }
        }
    }

    /// Returns true if `path` is explicitly represented under this node
    ///
    /// Explicit representation means there is a `Node` in the tree whose full path
    /// is equal to `path`. Implicit representation is only applicable to directories,
    /// and refers to when there is a `Node` in the tree for which `path` is a prefix,
    /// but the full path for that node refers to an item deeper in the directory tree,
    /// e.g. `/foo/bar` is implicitly represented by a node whose path is `/foo/bar/baz.txt`
    pub fn contains(&self, mut path: &Path) -> bool {
        let mut next = Some(self);
        while let Some(node) = next.take() {
            match node {
                Self::Branch {
                    ref component,
                    ref children,
                    ref data,
                } => {
                    if let Ok(rest) = path.strip_prefix(component) {
                        if rest == Path::new("") {
                            return data.is_some();
                        }
                        match children.iter().position(|c| c.has_common_prefix(rest)) {
                            Some(index) => {
                                path = rest;
                                next = Some(&children[index]);
                                continue;
                            }
                            None => {
                                // the remainder of `path` is not covered by any children
                                break;
                            }
                        }
                    } else {
                        // `component` may have a common prefix with `path`, but `path` is
                        // not explicitly represented in the tree, so we must return false
                        break;
                    }
                }
                Self::Leaf {
                    ref component,
                    ref data,
                    ..
                } => {
                    // `component` may have a common prefix with `path`, but unless `path`
                    // is equal to `component`, it is not explicitly represented in the tree
                    return path == component && data.is_some();
                }
            }
        }

        false
    }

    pub fn get<'a>(&'a self, mut path: &Path) -> Option<&'a Node> {
        let mut next = Some(self);
        while let Some(node) = next.take() {
            match node {
                Self::Branch {
                    ref component,
                    ref children,
                    ..
                } => {
                    if let Ok(rest) = path.strip_prefix(component) {
                        if rest == Path::new("") {
                            return Some(node);
                        }
                        match children.iter().position(|c| c.has_common_prefix(rest)) {
                            Some(index) => {
                                path = rest;
                                next = Some(&children[index]);
                                continue;
                            }
                            None => {
                                break;
                            }
                        }
                    }
                }
                Self::Leaf { ref component, .. } => {
                    if path == component {
                        return Some(node);
                    }
                    break;
                }
            }
        }

        None
    }

    /// Return the path and data key associated with it, which is the nearest
    /// ancestor of `path`, with data, represented under this node.
    ///
    /// For example, consider a tree in which `/foo/bar/baz/qux.txt` and
    /// `/foo/bar/qux2.txt` are both inserted:
    ///
    /// * Given `/foo/bar/baz/other.txt` this function will select `/foo/bar/qux2.txt`,
    /// because `/foo/bar/baz/qux.txt` is a sibling, but not an ancestor.
    /// * Given `/foo/bar/qux2.txt`, it will select None, because there is no ancestor
    /// explicitly represented in the tree for this path
    /// * Given `/foo/bar/baz/qux.txt`, it will select `/foo/bar/qux2.txt`, because
    /// `/foo/bar/baz/qux.txt` is not an ancestor of itself, and its nearest ancestor
    /// is the selected path.
    pub fn nearest_ancestor(&self, mut path: &Path) -> Option<(PathBuf, DataKey)> {
        use smallvec::SmallVec;
        if !self.has_common_prefix(path) {
            return None;
        }

        let mut candidates = SmallVec::<[(PathBuf, DataKey); 2]>::default();
        let mut ancestor = PathBuf::new();
        let mut next = Some(self);
        while let Some(node) = next.take() {
            match node {
                Self::Branch {
                    ref component,
                    ref children,
                    data,
                } => {
                    if let Ok(rest) = path.strip_prefix(component) {
                        // We found `path`, so take the most recent candidate,
                        // if one is available. If not, then there are no nearest
                        // ancestors for `path`
                        if rest == Path::new("") {
                            return candidates.pop();
                        }

                        // Otherwise, find the next child node to descend into.
                        // If a suitable candidate is found, and the current node
                        // has data associated with it, push the current path as
                        // a candidate node, and check the child node to see if it
                        // is closer.
                        //
                        // If no suitable candidate is found, but the current node
                        // has data associated with it, then the current node is the
                        // nearest ancestor, so we can just return it. If it has no
                        // data, then the most recent candidate is returned, if available.
                        ancestor.push(component);
                        let child = children.iter().position(|c| c.has_common_prefix(rest));
                        if let Some(index) = child {
                            if let Some(data) = *data {
                                candidates.push((ancestor.clone(), data));
                            }

                            path = rest;
                            next = Some(&children[index]);
                            continue;
                        } else {
                            return data
                                .map(|data| (ancestor, data))
                                .or_else(|| candidates.pop());
                        }
                    } else {
                        // To insert `path` in the tree, we'd have to split `node`, which
                        // means the most recent candidate is the nearest ancestor, if we
                        // have one
                        return candidates.pop();
                    }
                }
                // Leaf nodes by definition cannot be the nearest ancestor, so use
                // the most recent candidate, if we have one
                Self::Leaf { .. } => return candidates.pop(),
            }
        }

        None
    }

    fn take(&mut self, prefix: &Path) -> Self {
        match self {
            Node::Branch {
                ref component,
                ref mut children,
                ref mut data,
            } => {
                let component = component.strip_prefix(prefix).unwrap().to_path_buf();
                Node::Branch {
                    component,
                    children: core::mem::take(children),
                    data: data.take(),
                }
            }
            Node::Leaf {
                ref component,
                ref mut data,
                ty,
            } => {
                let component = component.strip_prefix(prefix).unwrap().to_path_buf();
                Node::Leaf {
                    component,
                    ty: *ty,
                    data: data.take(),
                }
            }
        }
    }

    fn split_at(&mut self, common_prefix: PathBuf) {
        let split = self.take(&common_prefix);
        *self = Node::Branch {
            component: common_prefix,
            children: vec![split],
            data: None,
        };
    }

    fn set_type(&mut self, ty: PathType) -> Result<(), TryInsertError> {
        match self {
            Self::Leaf {
                ref mut component,
                ref mut data,
                ty: ref mut prev_ty,
                ..
            } => match ty {
                PathType::Unknown | PathType::File => *prev_ty = ty,
                PathType::Dir => {
                    let component = core::mem::replace(component, PathBuf::new());
                    let data = data.take();
                    *self = Self::Branch {
                        component,
                        data,
                        children: vec![],
                    };
                }
            },
            Self::Branch { .. } => match ty {
                PathType::Dir => (),
                PathType::Unknown | PathType::File => return Err(TryInsertError::PathTypeConflict),
            },
        }

        Ok(())
    }

    fn set_data(&mut self, data: DataKey) {
        match self {
            Self::Branch {
                data: ref mut prev_data,
                ..
            }
            | Self::Leaf {
                data: ref mut prev_data,
                ..
            } => {
                *prev_data = Some(data);
            }
        }
    }

    fn push_child(
        &mut self,
        component: PathBuf,
        ty: PathType,
        data: Option<DataKey>,
    ) -> Result<(), TryInsertError> {
        let child = match ty {
            PathType::File | PathType::Unknown => Self::Leaf {
                component,
                ty,
                data,
            },
            PathType::Dir => Self::Branch {
                component,
                children: vec![],
                data,
            },
        };
        match self {
            Self::Branch {
                ref mut children, ..
            } => {
                children.push(child);
                children.sort();
            }
            Self::Leaf {
                component: ref mut parent_component,
                data: ref mut parent_data,
                ty: PathType::Unknown,
            } => {
                let children = vec![child];
                let component = core::mem::replace(parent_component, PathBuf::new());
                let data = parent_data.take();
                *self = Self::Branch {
                    component,
                    children,
                    data,
                };
            }
            Self::Leaf { .. } => return Err(TryInsertError::PathTypeConflict),
        }

        Ok(())
    }

    fn try_insert_new<'a>(
        &'a mut self,
        path: &Path,
        component: &Path,
        ty: PathType,
        data: Option<DataKey>,
    ) -> Either<Result<(), TryInsertError>, &'a mut Node> {
        match self {
            Self::Branch {
                ref mut children, ..
            } => {
                if let Some(index) = children.iter().position(|c| c.has_common_prefix(component)) {
                    // We can't insert this as new, but the given index is a child we can try to insert into next
                    return Right(&mut children[index]);
                }
                let child = match ty {
                    PathType::File | PathType::Unknown => Self::Leaf {
                        component: component.to_path_buf(),
                        ty,
                        data,
                    },
                    PathType::Dir => Self::Branch {
                        component: component.to_path_buf(),
                        children: vec![],
                        data,
                    },
                };
                children.push(child);
                children.sort();

                Left(Ok(()))
            }
            Self::Leaf {
                ty: PathType::File, ..
            } => Left(Err(TryInsertError::Unreachable(path.to_path_buf()))),
            Self::Leaf {
                component: ref mut parent_component,
                data: ref mut parent_data,
                ..
            } => {
                let child = match ty {
                    PathType::File | PathType::Unknown => Self::Leaf {
                        component: component.to_path_buf(),
                        ty,
                        data,
                    },
                    PathType::Dir => Self::Branch {
                        component: component.to_path_buf(),
                        children: vec![],
                        data,
                    },
                };
                let children = vec![child];
                let component = core::mem::replace(parent_component, PathBuf::new());
                let data = parent_data.take();
                *self = Self::Branch {
                    component,
                    children,
                    data,
                };
                Left(Ok(()))
            }
        }
    }
}

/// Insert `path` into `node` which contains a prefix of `path`.
///
/// Returns the depth relative to `node` where `path` is inserted, a depth
/// of zero indicates that `node` itself was modified.
#[inline(never)]
fn insert_into_prefix(
    node: &mut Node,
    mut path: &Path,
    ty: PathType,
    data: DataKey,
) -> Result<usize, TryInsertError> {
    let orig_path = path;
    let mut next = Some((node, 0));

    loop {
        let (node, depth) = next.take().unwrap();

        if let Some(prefix) = node.common_prefix(path) {
            match prefix {
                Prefix::Exact => {
                    node.set_type(ty)?;
                    node.set_data(data);
                    break Ok(depth);
                }
                Prefix::Child => {
                    path = path.strip_prefix(node.component()).unwrap();
                    match node.try_insert_new(orig_path, path, ty, Some(data)) {
                        Left(result) => {
                            result?;
                            break Ok(depth + 1);
                        }
                        Right(next_child) => {
                            next = Some((next_child, depth + 1));
                            continue;
                        }
                    }
                }
                Prefix::Ancestor => {
                    node.split_at(path.to_path_buf());
                    node.set_data(data);
                    break Ok(depth);
                }
                Prefix::Partial(common_prefix) => {
                    let component = path.strip_prefix(&common_prefix).unwrap().to_path_buf();
                    node.split_at(common_prefix);
                    node.push_child(component, ty, Some(data))?;
                    break Ok(depth);
                }
            }
        }
    }
}

#[derive(Debug)]
struct DfsVisitor<'a> {
    worklist: &'a [Node],
    prefix: PathBuf,
    component_prefix: PathBuf,
    queued: Option<(PathBuf, usize, &'a [Node])>,
    depth: usize,
    only_leaves: bool,
}
impl<'a> DfsVisitor<'a> {
    fn new(prefix: PathBuf, worklist: &'a [Node], depth: usize, only_leaves: bool) -> Self {
        Self {
            worklist,
            prefix,
            component_prefix: PathBuf::new(),
            queued: None,
            depth,
            only_leaves,
        }
    }

    pub fn next(
        &mut self,
        max_depth: Option<usize>,
    ) -> Option<Either<(PathBuf, DataKey), DfsVisitor<'a>>> {
        if let Some((prefix, relative_depth, worklist)) = self.queued.take() {
            return Some(Right(DfsVisitor::new(
                prefix,
                worklist,
                relative_depth,
                self.only_leaves,
            )));
        }

        loop {
            match self.worklist.split_first()? {
                (Node::Leaf { data: None, .. }, worklist) => {
                    self.worklist = worklist;
                    continue;
                }
                (
                    Node::Leaf {
                        ref component,
                        data: Some(data),
                        ..
                    },
                    worklist,
                ) => {
                    self.worklist = worklist;

                    if self.exceeds_max_depth(component, max_depth) {
                        continue;
                    }
                    let suffix = self.strip_prefix(component);
                    let prefix = self.prefix.join(suffix);
                    break Some(Left((prefix, *data)));
                }
                (
                    Node::Branch {
                        ref component,
                        ref children,
                        data,
                    },
                    worklist,
                ) => {
                    self.worklist = worklist;

                    let relative_depth = self.relative_depth(component, max_depth);
                    if let Some(max_depth) = max_depth {
                        if relative_depth > max_depth {
                            continue;
                        }
                    }
                    let suffix = self.strip_prefix(component);
                    let prefix = self.prefix.join(suffix);
                    if !children.is_empty() {
                        match data {
                            // We have to emit the branch node first, so save the descent
                            // information for the next time `next` is called
                            Some(data) => {
                                self.queued =
                                    Some((prefix.clone(), relative_depth, children.as_slice()));
                                break Some(Left((prefix, *data)));
                            }
                            // We don't need to emit the branch node, so return a new visitor
                            // to start descending into the children
                            None => {
                                break Some(Right(DfsVisitor::new(
                                    prefix,
                                    children.as_slice(),
                                    relative_depth,
                                    self.only_leaves,
                                )));
                            }
                        }
                    }
                }
            }
        }
    }

    fn exceeds_max_depth(&self, path: &Path, max_depth: Option<usize>) -> bool {
        match max_depth {
            None => false,
            Some(max_depth) => {
                let suffix = self.strip_prefix(path);
                let relative_depth = self.depth + suffix.components().count();
                relative_depth > max_depth
            }
        }
    }

    fn relative_depth(&self, path: &Path, max_depth: Option<usize>) -> usize {
        match max_depth {
            // If we don't care about the depth, do nothing
            None => 0,
            Some(_) => {
                let suffix = self.strip_prefix(path);
                self.depth + suffix.components().count()
            }
        }
    }

    #[inline]
    fn strip_prefix<'p>(&self, path: &'p Path) -> &'p Path {
        if self.component_prefix == Path::new("") {
            return path;
        }
        path.strip_prefix(&self.component_prefix).unwrap()
    }
}

struct Dfs<'a, V> {
    data: &'a [V],
    stack: Vec<DfsVisitor<'a>>,
    current: DfsVisitor<'a>,
    max_depth: Option<usize>,
}
impl<V> core::fmt::Debug for Dfs<'_, V> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.debug_struct("Dfs")
            .field("stack", &self.stack)
            .field("current", &self.current)
            .field("max_depth", &self.max_depth)
            .finish()
    }
}
impl<'a, V> Dfs<'a, V> {
    fn new(tree: &'a PathPrefixTree<V>, only_leaves: bool, max_depth: Option<usize>) -> Self {
        Self {
            data: tree.data.as_slice(),
            stack: vec![],
            current: DfsVisitor::new(PathBuf::new(), tree.tree.roots.as_slice(), 0, only_leaves),
            max_depth,
        }
    }

    fn at(
        tree: &'a PathPrefixTree<V>,
        path: &Path,
        only_leaves: bool,
        max_depth: Option<usize>,
    ) -> Self {
        // Traverse tree until we find our initial working set
        match tree
            .tree
            .roots
            .iter()
            .find(|root| root.has_common_prefix(path))
        {
            None => return Self::empty(tree, only_leaves, max_depth),
            Some(root) => {
                let mut next = Some(root);
                let mut input_path = path;
                let mut prefix = PathBuf::new();
                loop {
                    let node = next.take().unwrap();
                    match node.common_prefix(input_path) {
                        // No children for `path` in this tree
                        None => break Self::empty(tree, only_leaves, max_depth),
                        Some(Prefix::Exact) => {
                            break Self {
                                data: tree.data.as_slice(),
                                stack: vec![],
                                current: DfsVisitor::new(
                                    prefix,
                                    core::slice::from_ref(node),
                                    0,
                                    only_leaves,
                                ),
                                max_depth,
                            };
                        }
                        // This node is the first child, as long as it is not deeper than `max_depth`
                        Some(Prefix::Ancestor) => {
                            prefix.push(input_path);
                            break Self {
                                data: tree.data.as_slice(),
                                stack: vec![],
                                current: DfsVisitor {
                                    component_prefix: PathBuf::from(input_path),
                                    ..DfsVisitor::new(
                                        prefix,
                                        core::slice::from_ref(node),
                                        0,
                                        only_leaves,
                                    )
                                },
                                max_depth,
                            };
                        }
                        // We need to recurse deeper into the tree to find potential children
                        Some(Prefix::Child) => {
                            match node {
                                Node::Branch {
                                    ref component,
                                    ref children,
                                    ..
                                } => {
                                    input_path = input_path.strip_prefix(component).unwrap();
                                    next =
                                        children.iter().find(|c| c.has_common_prefix(input_path));
                                    if next.is_none() {
                                        break Self::empty(tree, only_leaves, max_depth);
                                    }
                                    prefix.push(component);
                                }
                                // There are no children for `path`, so return an empty iterator
                                Node::Leaf { .. } => {
                                    break Self::empty(tree, only_leaves, max_depth)
                                }
                            }
                        }
                        // There can be no children of `path` under `node`
                        Some(Prefix::Partial(_)) => {
                            break Self::empty(tree, only_leaves, max_depth)
                        }
                    }
                }
            }
        }
    }

    #[inline]
    fn empty(tree: &'a PathPrefixTree<V>, only_leaves: bool, max_depth: Option<usize>) -> Self {
        Self {
            data: tree.data.as_slice(),
            stack: vec![],
            current: DfsVisitor::new(PathBuf::new(), &[], 0, only_leaves),
            max_depth,
        }
    }
}
impl<'a, V> Iterator for Dfs<'a, V> {
    type Item = Entry<'a, V>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.current.next(self.max_depth) {
                // No more nodes at this depth
                None => {
                    // Try to resume iteration at the next level up, else we're done
                    self.current = self.stack.pop()?;
                }
                // Visitor produced the next element at this depth
                Some(Left((path, key))) => {
                    return Some(Entry {
                        path,
                        data: &self.data[key.as_usize()],
                    });
                }
                // Visitor has indicated we should suspend iteration at this
                // depth and descend into a child node first
                Some(Right(visitor)) => {
                    let suspended = core::mem::replace(&mut self.current, visitor);
                    self.stack.push(suspended);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn path_tree_insert() {
        // Tests:
        // * insertion in a clean tree
        // * insertion under an existing node
        // * insertion in a dirty tree
        let tree = PathPrefixTree::<()>::from_iter(["/foo/bar", "/foo/bar/baz", "/qux"]);

        let child = Node::Leaf {
            component: PathBuf::from("baz"),
            ty: PathType::Unknown,
            data: Some(DataKey(1)),
        };
        let a = Node::Branch {
            component: PathBuf::from("foo"),
            children: vec![Node::Branch {
                component: PathBuf::from("bar"),
                children: vec![child],
                data: Some(DataKey(0)),
            }],
            data: None,
        };
        let b = Node::Leaf {
            component: PathBuf::from("qux"),
            ty: PathType::Unknown,
            data: Some(DataKey(2)),
        };
        let root = Node::Branch {
            component: PathBuf::from("/"),
            children: vec![a, b],
            data: None,
        };

        assert_eq!(tree.tree.roots.as_slice(), &[root]);
    }

    #[test]
    fn path_tree_nearest_ancestor() {
        let tree = PathPrefixTree::<()>::from_iter(["/foo/bar", "/foo/bar/baz", "/qux"]);

        assert_eq!(
            tree.nearest_ancestor("/foo/bar/baz").map(Entry::into_path),
            Some(PathBuf::from("/foo/bar"))
        );
        assert_eq!(
            tree.nearest_ancestor("/foo/bar").map(Entry::into_path),
            None
        );
        assert_eq!(tree.nearest_ancestor("/qux").map(Entry::into_path), None);
    }

    #[test]
    fn path_tree_contains() {
        let tree = PathPrefixTree::<()>::from_iter(["/foo/bar", "/foo/bar/baz", "/qux"]);

        assert!(tree.contains("/foo/bar/baz"));
        assert!(tree.contains("/foo/bar"));
        assert!(!tree.contains("/foo"));
        assert!(!tree.contains("/foo/bar/baz/thing.txt"));
    }

    #[test]
    fn path_tree_get() {
        let mut tree = PathPrefixTree::default();
        tree.insert("/foo/bar/baz", 1usize);
        tree.insert("/foo/bar/baz/qux", 2usize);

        assert_eq!(tree.get("/foo/bar/baz/qux").copied(), Some(2));
        assert_eq!(tree.get("/foo/bar/baz").copied(), Some(1));
    }

    #[test]
    fn path_tree_iter() {
        let tree = PathPrefixTree::<()>::from_iter(["/qux", "/foo/bar/baz", "/foo/bar"]);

        let paths = tree.iter().map(|e| e.into_path()).collect::<Vec<_>>();

        let expected = vec![
            PathBuf::from("/foo/bar"),
            PathBuf::from("/foo/bar/baz"),
            PathBuf::from("/qux"),
        ];

        assert_eq!(paths, expected);
    }

    #[test]
    fn path_tree_children() {
        let tree = PathPrefixTree::<()>::from_iter(["/qux", "/foo/bar/baz", "/foo/bar"]);

        let paths = tree
            .children("/foo/bar", None)
            .map(|e| e.into_path())
            .collect::<Vec<_>>();

        let expected = vec![PathBuf::from("/foo/bar"), PathBuf::from("/foo/bar/baz")];

        assert_eq!(paths, expected);

        let paths = tree
            .children("/foo", None)
            .map(|e| e.into_path())
            .collect::<Vec<_>>();

        assert_eq!(paths, expected);
    }
}
