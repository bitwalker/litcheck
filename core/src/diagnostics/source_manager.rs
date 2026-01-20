use core::{error::Error, fmt::Debug};
use std::{collections::BTreeMap, sync::Arc};

use serde::{Deserialize, Serialize};

use super::*;
use crate::range::Range;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u32)]
pub enum SourceGroup {
    File = 0,
    Argument = u32::MAX - 1,
    Unknown = u32::MAX,
}

/// A [SourceId] represents the index/identifier associated with a unique source file in a
/// [SourceManager] implementation.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SourceId(u32);

impl From<u32> for SourceId {
    fn from(value: u32) -> Self {
        SourceId::new_unchecked(value)
    }
}

impl Default for SourceId {
    fn default() -> Self {
        Self::UNKNOWN
    }
}

impl SourceId {
    pub const UNKNOWN: Self = Self(u32::MAX);

    const GROUP_MASK: u32 = (u8::MAX as u32) << 24;
    const INDEX_MASK: u32 = !Self::GROUP_MASK;
    const MAX_INDEX: u32 = Self::INDEX_MASK;

    /// Create a new [SourceId] representing a file, from a `u32` value, but assert if out of range
    pub fn file(id: u32) -> Self {
        assert!(
            id <= Self::MAX_INDEX,
            "source id out of range: only 2^24 unique ids can be represented"
        );

        Self(id | SourceGroup::File as u32)
    }

    /// Create a new [SourceId] representing a command-line argument, from a `u32` value, but assert
    /// if out of range
    pub fn argument(id: u32) -> Self {
        assert!(
            id <= Self::MAX_INDEX,
            "source id out of range: only 2^24 unique ids can be represented"
        );

        Self(id | SourceGroup::Argument as u32)
    }

    /// Create a new [SourceId] from a raw `u32` value
    #[inline(always)]
    pub const fn new_unchecked(id: u32) -> Self {
        assert!(
            id & Self::GROUP_MASK == (SourceGroup::File as u32),
            "invalid source id group: expected file"
        );
        Self(id)
    }

    #[inline(always)]
    pub const fn to_index(self) -> usize {
        (self.0 & Self::INDEX_MASK) as usize
    }

    #[inline(always)]
    pub const fn group(&self) -> SourceGroup {
        let group = self.0 & Self::GROUP_MASK;
        // SAFETY: We ensure SourceId is only ever constructed with a valid group
        unsafe { *(&group as *const u32).cast::<SourceGroup>() }
    }

    #[inline(always)]
    pub const fn to_u32(self) -> u32 {
        self.0
    }

    pub const fn is_unknown(&self) -> bool {
        self.0 == Self::UNKNOWN.0
    }

    pub const fn is_cli_argument(&self) -> bool {
        matches!(self.group(), SourceGroup::Argument)
    }
}

impl TryFrom<usize> for SourceId {
    type Error = ();

    #[inline]
    fn try_from(id: usize) -> Result<Self, Self::Error> {
        match u32::try_from(id) {
            Ok(n) if n & SourceId::GROUP_MASK == 0 => Ok(Self(n)),
            _ => Err(()),
        }
    }
}

// SOURCE MANAGER
// ================================================================================================

/// The set of errors which may be raised by a [SourceManager]
#[derive(Debug, thiserror::Error)]
pub enum SourceManagerError {
    /// A [SourceId] was provided to a [SourceManager] which was allocated by a different
    /// [SourceManager]
    #[error("attempted to use an invalid source id")]
    InvalidSourceId,
    /// An attempt was made to read content using invalid byte indices
    #[error("attempted to read content out of bounds")]
    InvalidBounds,
    #[error(transparent)]
    InvalidContentUpdate(#[from] SourceContentUpdateError),
    /// Custom error variant for implementors of the trait.
    #[error("{error_msg}")]
    Custom {
        error_msg: Box<str>,
        // thiserror will return this when calling Error::source on SourceManagerError.
        source: Option<Box<dyn Error + Send + Sync + 'static>>,
    },
}

impl SourceManagerError {
    pub fn custom(message: String) -> Self {
        Self::Custom {
            error_msg: message.into(),
            source: None,
        }
    }

    pub fn custom_with_source(message: String, source: impl Error + Send + Sync + 'static) -> Self {
        Self::Custom {
            error_msg: message.into(),
            source: Some(Box::new(source)),
        }
    }
}

pub trait SourceManager: Debug {
    /// Returns true if `file` is managed by this source manager
    fn is_manager_of(&self, file: &SourceFile) -> bool {
        match self.get(file.id()) {
            Ok(found) => core::ptr::addr_eq(Arc::as_ptr(&found), file),
            Err(_) => false,
        }
    }
    /// Copies `file` into this source manager (if not already managed by this manager).
    ///
    /// The returned source file is guaranteed to be owned by this manager.
    fn copy_into(&self, file: &SourceFile) -> Arc<SourceFile> {
        if let Ok(found) = self.get(file.id())
            && core::ptr::addr_eq(Arc::as_ptr(&found), file)
        {
            return found;
        }
        self.load_from_raw_parts(
            file.id().group(),
            file.uri().clone(),
            file.content().clone(),
        )
    }
    fn load_argument(&self, name: &'static str, content: String) -> Arc<SourceFile> {
        let content = SourceContent::new(SourceLanguage::Unknown, name, content);
        self.load_from_raw_parts(
            SourceGroup::Argument,
            FileName::from_static_str(name),
            content,
        )
    }
    /// Load the given `content` into this [SourceManager] with `name`
    fn load(&self, lang: SourceLanguage, name: FileName, content: String) -> Arc<SourceFile> {
        let content = SourceContent::new(lang, name.clone(), content);
        self.load_from_raw_parts(SourceGroup::File, name, content)
    }
    /// Load content into this [SourceManager] from raw [SourceFile] components
    fn load_from_raw_parts(
        &self,
        group: SourceGroup,
        name: FileName,
        content: SourceContent,
    ) -> Arc<SourceFile>;
    /// Update the source file corresponding to `id` after being notified of a change event.
    ///
    /// The `version` indicates the new version of the document
    fn update(
        &self,
        id: SourceId,
        text: String,
        range: Option<Selection>,
        version: i32,
    ) -> Result<(), SourceManagerError>;
    /// Get the [SourceFile] corresponding to `id`
    fn get(&self, id: SourceId) -> Result<Arc<SourceFile>, SourceManagerError>;
    /// Get the most recent [SourceFile] whose URI is `uri`
    fn get_by_uri(&self, uri: &FileName) -> Option<Arc<SourceFile>> {
        self.find(uri).and_then(|id| self.get(id).ok())
    }
    /// Search for a source file whose URI is `uri`, and return its [SourceId] if found.
    fn find(&self, uri: &FileName) -> Option<SourceId>;
    /// Convert a [FileLineCol] to an equivalent [SourceSpan], if the referenced file is available
    fn file_line_col_to_span(&self, loc: FileLineCol) -> Option<SourceSpan>;
    /// Convert a [SourceSpan] to an equivalent [FileLineCol], if the span is valid
    fn file_line_col(&self, span: SourceSpan) -> Result<FileLineCol, SourceManagerError>;
    /// Convert a [Location] to an equivalent [SourceSpan], if the referenced file is available
    fn location_to_span(&self, loc: Location) -> Option<SourceSpan>;
    /// Convert a [SourceSpan] to an equivalent [Location], if the span is valid
    fn location(&self, span: SourceSpan) -> Result<Location, SourceManagerError>;
    /// Get the source associated with `id` as a string slice
    fn source(&self, id: SourceId) -> Result<&str, SourceManagerError>;
    /// Get the source corresponding to `span` as a string slice
    fn source_slice(&self, span: SourceSpan) -> Result<&str, SourceManagerError>;
}

impl<T: ?Sized + SourceManager> SourceManager for Arc<T> {
    #[inline(always)]
    fn is_manager_of(&self, file: &SourceFile) -> bool {
        (**self).is_manager_of(file)
    }
    #[inline(always)]
    fn copy_into(&self, file: &SourceFile) -> Arc<SourceFile> {
        (**self).copy_into(file)
    }
    #[inline(always)]
    fn load(&self, lang: SourceLanguage, uri: FileName, content: String) -> Arc<SourceFile> {
        (**self).load(lang, uri, content)
    }
    #[inline(always)]
    fn load_from_raw_parts(
        &self,
        group: SourceGroup,
        uri: FileName,
        content: SourceContent,
    ) -> Arc<SourceFile> {
        (**self).load_from_raw_parts(group, uri, content)
    }
    #[inline(always)]
    fn update(
        &self,
        id: SourceId,
        text: String,
        range: Option<Selection>,
        version: i32,
    ) -> Result<(), SourceManagerError> {
        (**self).update(id, text, range, version)
    }
    #[inline(always)]
    fn get(&self, id: SourceId) -> Result<Arc<SourceFile>, SourceManagerError> {
        (**self).get(id)
    }
    #[inline(always)]
    fn get_by_uri(&self, uri: &FileName) -> Option<Arc<SourceFile>> {
        (**self).get_by_uri(uri)
    }
    #[inline(always)]
    fn find(&self, uri: &FileName) -> Option<SourceId> {
        (**self).find(uri)
    }
    #[inline(always)]
    fn file_line_col_to_span(&self, loc: FileLineCol) -> Option<SourceSpan> {
        (**self).file_line_col_to_span(loc)
    }
    #[inline(always)]
    fn file_line_col(&self, span: SourceSpan) -> Result<FileLineCol, SourceManagerError> {
        (**self).file_line_col(span)
    }
    #[inline(always)]
    fn location_to_span(&self, loc: Location) -> Option<SourceSpan> {
        (**self).location_to_span(loc)
    }
    #[inline(always)]
    fn location(&self, span: SourceSpan) -> Result<Location, SourceManagerError> {
        (**self).location(span)
    }
    #[inline(always)]
    fn source(&self, id: SourceId) -> Result<&str, SourceManagerError> {
        (**self).source(id)
    }
    #[inline(always)]
    fn source_slice(&self, span: SourceSpan) -> Result<&str, SourceManagerError> {
        (**self).source_slice(span)
    }
}

pub trait SourceManagerExt: SourceManager {
    /// Load the content of `path` into this [SourceManager]
    fn load_file(&self, path: &std::path::Path) -> Result<Arc<SourceFile>, SourceManagerError> {
        let uri = FileName::from(path);
        if let Some(existing) = self.get_by_uri(&uri) {
            return Ok(existing);
        }

        let lang = SourceLanguage::from_path(path);
        let content = std::fs::read_to_string(path)
            .map(|s| SourceContent::new(lang, uri.clone(), s))
            .map_err(|source| {
                SourceManagerError::custom_with_source(
                    format!("failed to load filed at `{}`", path.display()),
                    source,
                )
            })?;

        Ok(self.load_from_raw_parts(SourceGroup::File, uri, content))
    }
}

impl<T: ?Sized + SourceManager> SourceManagerExt for T {}

/// [SourceManagerSync] is a marker trait for [SourceManager] implementations that are also Send +
/// Sync, and is automatically implemented for any [SourceManager] that meets those requirements.
///
/// [SourceManager] is a supertrait of [SourceManagerSync], so you may use instances of the
/// [SourceManagerSync] where the [SourceManager] is required, either implicitly or via explicit
/// downcasting, e.g. `Arc<dyn SourceManagerSync> as Arc<dyn SourceManager>`.
pub trait SourceManagerSync: SourceManager + Send + Sync {}

impl<T: ?Sized + SourceManager + Send + Sync> SourceManagerSync for T {}

// DEFAULT SOURCE MANAGER
// ================================================================================================

use parking_lot::RwLock;

#[derive(Debug, Default)]
pub struct DefaultSourceManager(RwLock<DefaultSourceManagerImpl>);

impl Clone for DefaultSourceManager {
    fn clone(&self) -> Self {
        let manager = self.0.read();
        Self(RwLock::new(manager.clone()))
    }
}

impl Clone for DefaultSourceManagerImpl {
    fn clone(&self) -> Self {
        Self {
            files: self.files.clone(),
            arguments: self.arguments.clone(),
            uris: self.uris.clone(),
        }
    }
}

#[derive(Debug, Default)]
struct DefaultSourceManagerImpl {
    files: Vec<Arc<SourceFile>>,
    arguments: Vec<Arc<SourceFile>>,
    uris: BTreeMap<FileName, SourceId>,
}

impl DefaultSourceManagerImpl {
    fn insert(
        &mut self,
        group: SourceGroup,
        uri: FileName,
        content: SourceContent,
    ) -> Arc<SourceFile> {
        // If we have previously inserted the same content with `name`, return the previously
        // inserted source id
        if let Some(file) = self.uris.get(&uri).copied().and_then(|id| {
            let file = if id.is_cli_argument() {
                &self.arguments[id.to_index()]
            } else {
                &self.files[id.to_index()]
            };
            if file.as_str() == content.as_str() {
                Some(Arc::clone(file))
            } else {
                None
            }
        }) {
            return file;
        }
        match group {
            SourceGroup::File => {
                let index = u32::try_from(self.files.len())
                    .expect("system limit: too many source files tracked");
                let id = SourceId::file(index);
                let file = Arc::new(SourceFile::from_raw_parts(id, content));
                self.files.push(Arc::clone(&file));
                self.uris.insert(uri, id);
                file
            }
            SourceGroup::Argument => {
                let index = u32::try_from(self.files.len())
                    .expect("system limit: too many argument sources tracked");
                let id = SourceId::argument(index);
                let file = Arc::new(SourceFile::from_raw_parts(id, content));
                self.arguments.push(Arc::clone(&file));
                self.uris.insert(uri, id);
                file
            }
            SourceGroup::Unknown => unreachable!(),
        }
    }

    fn get(&self, id: SourceId) -> Result<Arc<SourceFile>, SourceManagerError> {
        if id.is_cli_argument() {
            self.arguments
                .get(id.to_index())
                .cloned()
                .ok_or(SourceManagerError::InvalidSourceId)
        } else {
            self.files
                .get(id.to_index())
                .cloned()
                .ok_or(SourceManagerError::InvalidSourceId)
        }
    }

    fn get_by_uri(&self, uri: &FileName) -> Option<Arc<SourceFile>> {
        self.find(uri).and_then(|id| self.get(id).ok())
    }

    fn find(&self, uri: &FileName) -> Option<SourceId> {
        self.uris.get(uri).copied()
    }

    fn file_line_col_to_span(&self, loc: FileLineCol) -> Option<SourceSpan> {
        let file = self
            .uris
            .get(&loc.uri)
            .copied()
            .and_then(|id| self.get(id).ok())?;
        file.line_column_to_span(loc.line, loc.column)
    }

    fn file_line_col(&self, span: SourceSpan) -> Result<FileLineCol, SourceManagerError> {
        self.get(span.source_id()).map(|file| file.location(span))
    }

    fn location_to_span(&self, loc: Location) -> Option<SourceSpan> {
        let file = self
            .uris
            .get(&loc.uri)
            .copied()
            .and_then(|id| self.get(id).ok())?;

        let max_len = ByteIndex::from(file.as_str().len() as u32);
        if loc.start >= max_len || loc.end > max_len {
            return None;
        }

        Some(SourceSpan::new(file.id(), Range::new(loc.start, loc.end)))
    }

    fn location(&self, span: SourceSpan) -> Result<Location, SourceManagerError> {
        self.get(span.source_id())
            .map(|file| Location::new(file.uri().clone(), span.start(), span.end()))
    }
}

impl SourceManager for DefaultSourceManager {
    fn load_from_raw_parts(
        &self,
        group: SourceGroup,
        uri: FileName,
        content: SourceContent,
    ) -> Arc<SourceFile> {
        let mut manager = self.0.write();
        manager.insert(group, uri, content)
    }

    fn update(
        &self,
        id: SourceId,
        text: String,
        range: Option<Selection>,
        version: i32,
    ) -> Result<(), SourceManagerError> {
        let mut manager = self.0.write();
        match id.group() {
            SourceGroup::File => {
                let source_file = &mut manager.files[id.to_index()];
                let source_file_cloned = Arc::make_mut(source_file);
                source_file_cloned
                    .content_mut()
                    .update(text, range, version)
                    .map_err(SourceManagerError::InvalidContentUpdate)
            }
            SourceGroup::Argument => {
                let source_file = &mut manager.arguments[id.to_index()];
                let source_file_cloned = Arc::make_mut(source_file);
                source_file_cloned
                    .content_mut()
                    .update(text, range, version)
                    .map_err(SourceManagerError::InvalidContentUpdate)
            }
            SourceGroup::Unknown => Err(SourceManagerError::InvalidSourceId),
        }
    }

    fn get(&self, id: SourceId) -> Result<Arc<SourceFile>, SourceManagerError> {
        let manager = self.0.read();
        manager.get(id)
    }

    fn get_by_uri(&self, uri: &FileName) -> Option<Arc<SourceFile>> {
        let manager = self.0.read();
        manager.get_by_uri(uri)
    }

    fn find(&self, uri: &FileName) -> Option<SourceId> {
        let manager = self.0.read();
        manager.find(uri)
    }

    fn file_line_col_to_span(&self, loc: FileLineCol) -> Option<SourceSpan> {
        let manager = self.0.read();
        manager.file_line_col_to_span(loc)
    }

    fn file_line_col(&self, span: SourceSpan) -> Result<FileLineCol, SourceManagerError> {
        let manager = self.0.read();
        manager.file_line_col(span)
    }

    fn location_to_span(&self, loc: Location) -> Option<SourceSpan> {
        let manager = self.0.read();
        manager.location_to_span(loc)
    }

    fn location(&self, span: SourceSpan) -> Result<Location, SourceManagerError> {
        let manager = self.0.read();
        manager.location(span)
    }

    fn source(&self, id: SourceId) -> Result<&str, SourceManagerError> {
        let manager = self.0.read();
        let ptr = manager.get(id).map(|file| file.as_str() as *const str)?;
        drop(manager);
        // SAFETY: Because the lifetime of the returned reference is bound to the manager, and
        // because we can only ever add files, not modify/remove them, this is safe. Exclusive
        // access to the manager does _not_ mean exclusive access to the contents of previously
        // added source files
        Ok(unsafe { &*ptr })
    }

    fn source_slice(&self, span: SourceSpan) -> Result<&str, SourceManagerError> {
        self.source(span.source_id())?
            .get(span.into_slice_index().into_range())
            .ok_or(SourceManagerError::InvalidBounds)
    }
}

#[cfg(test)]
mod error_assertions {
    use super::*;

    /// Asserts at compile time that the passed error has Send + Sync + 'static bounds.
    fn _assert_error_is_send_sync_static<E: core::error::Error + Send + Sync + 'static>(_: E) {}

    fn _assert_source_manager_error_bounds(err: SourceManagerError) {
        _assert_error_is_send_sync_static(err);
    }
}
