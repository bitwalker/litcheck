use std::hash::{Hash, Hasher};

pub use miette::{
    bail, diagnostic, ByteOffset, Diagnostic, IntoDiagnostic, LabeledSpan, Report, Severity,
    SourceCode, SourceOffset, SourceSpan, WrapErr,
};

pub mod reporting {
    pub use miette::{
        set_hook, DebugReportHandler, JSONReportHandler, NarratableReportHandler, ReportHandler,
    };

    #[cfg(feature = "fancy-diagnostics")]
    pub use miette::{GraphicalReportHandler, GraphicalTheme};
    #[cfg(feature = "fancy-diagnostics")]
    pub type ReportHandlerOpts = miette::MietteHandlerOpts;
    #[cfg(feature = "fancy-diagnostics")]
    pub type DefaultReportHandler = miette::GraphicalReportHandler;
    #[cfg(not(feature = "fancy-diagnostics"))]
    pub type DefaultReportHandler = miette::DebugReportHandler;

    pub struct PrintDiagnostic<D, R = DefaultReportHandler> {
        handler: R,
        diag: D,
    }
    impl<D: AsRef<dyn super::Diagnostic>> PrintDiagnostic<D> {
        pub fn new(diag: D) -> Self {
            Self {
                handler: Default::default(),
                diag,
            }
        }
        #[cfg(feature = "fancy-diagnostics")]
        pub fn new_without_color(diag: D) -> Self {
            Self {
                handler: DefaultReportHandler::new_themed(GraphicalTheme::none()),
                diag,
            }
        }
        #[cfg(not(feature = "fancy-diagnostics"))]
        pub fn new_without_color(diag: D) -> Self {
            Self::new(diag)
        }
    }
    impl<D: AsRef<dyn super::Diagnostic>> PrintDiagnostic<D, NarratableReportHandler> {
        pub fn narrated(diag: D) -> Self {
            Self {
                handler: NarratableReportHandler::default(),
                diag,
            }
        }
    }
    impl<D: AsRef<dyn super::Diagnostic>> PrintDiagnostic<D, JSONReportHandler> {
        pub fn json(diag: D) -> Self {
            Self {
                handler: JSONReportHandler,
                diag,
            }
        }
    }
    impl<D: AsRef<dyn super::Diagnostic>> core::fmt::Display for PrintDiagnostic<D> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            self.handler.render_report(f, self.diag.as_ref())
        }
    }
    impl<D: AsRef<dyn super::Diagnostic>> core::fmt::Display
        for PrintDiagnostic<D, NarratableReportHandler>
    {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            self.handler.render_report(f, self.diag.as_ref())
        }
    }
    impl<D: AsRef<dyn super::Diagnostic>> core::fmt::Display for PrintDiagnostic<D, JSONReportHandler> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            self.handler.render_report(f, self.diag.as_ref())
        }
    }
}

#[cfg(feature = "fancy-diagnostics")]
pub use miette::set_panic_hook;

pub type Diag = miette::MietteDiagnostic;
pub type DiagResult<T> = miette::Result<T>;

use std::{
    borrow::{Borrow, Cow},
    convert::{AsMut, AsRef},
    fmt,
    ops::{Deref, DerefMut},
    path::Path,
    sync::Arc,
};

use miette::{MietteError, SpanContents};

use crate::{range::Range, StaticCow};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FileName {
    Stdin,
    Path(Box<Path>),
    Virtual(StaticCow<str>),
}
impl FileName {
    pub fn from_static_str(name: &'static str) -> Self {
        Self::Virtual(Cow::Borrowed(name))
    }

    pub fn name(&self) -> &str {
        match self {
            Self::Stdin => "stdin",
            Self::Path(ref path) => path.to_str().unwrap_or("<invalid>"),
            Self::Virtual(ref name) => name.as_ref(),
        }
    }
}
impl From<&Path> for FileName {
    fn from(path: &Path) -> Self {
        if path.as_os_str() == "-" {
            Self::Stdin
        } else {
            Self::Path(path.to_path_buf().into_boxed_path())
        }
    }
}
impl From<Box<Path>> for FileName {
    fn from(path: Box<Path>) -> Self {
        if path.as_os_str() == "-" {
            Self::Stdin
        } else {
            Self::Path(path)
        }
    }
}
impl From<std::path::PathBuf> for FileName {
    fn from(path: std::path::PathBuf) -> Self {
        if path.as_os_str() == "-" {
            Self::Stdin
        } else {
            Self::Path(path.into_boxed_path())
        }
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
            Self::Path(ref path) => write!(f, "{}", path.display()),
            Self::Virtual(ref name) => f.write_str(name),
        }
    }
}

pub trait NamedSourceFile: SourceFile {
    fn name(&self) -> FileName {
        FileName::Stdin
    }
}

pub trait SourceFile {
    fn span(&self) -> SourceSpan;
    fn source(&self) -> &str;
    #[inline]
    fn source_bytes(&self) -> &[u8] {
        self.source().as_bytes()
    }
}
impl SourceFile for str {
    fn span(&self) -> SourceSpan {
        let len = self.as_bytes().len();
        SourceSpan::from(0..len)
    }
    fn source(&self) -> &str {
        self
    }
}
impl NamedSourceFile for str {}
impl SourceFile for String {
    fn span(&self) -> SourceSpan {
        let len = self.as_bytes().len();
        SourceSpan::from(0..len)
    }
    fn source(&self) -> &str {
        self.as_str()
    }
}
impl NamedSourceFile for String {}
impl SourceFile for Box<str> {
    fn span(&self) -> SourceSpan {
        (**self).span()
    }
    fn source(&self) -> &str {
        self.as_ref()
    }
}
impl NamedSourceFile for Box<str> {}
impl<'a> SourceFile for Cow<'a, str> {
    fn span(&self) -> SourceSpan {
        (**self).span()
    }
    fn source(&self) -> &str {
        self.as_ref()
    }
}
impl<'a> NamedSourceFile for Cow<'a, str> {}
impl SourceFile for Arc<str> {
    fn span(&self) -> SourceSpan {
        (**self).span()
    }
    fn source(&self) -> &str {
        self.as_ref()
    }
}
impl NamedSourceFile for Arc<str> {}

#[derive(Debug, Clone)]
pub struct ArcSource(Arc<Source<'static>>);
impl From<String> for ArcSource {
    fn from(s: String) -> Self {
        Self::new(Source::from(s))
    }
}
impl From<&'static str> for ArcSource {
    fn from(s: &'static str) -> Self {
        Self::new(Source::new(FileName::Stdin, Cow::Borrowed(s)))
    }
}
impl From<Source<'static>> for ArcSource {
    fn from(source: Source<'static>) -> Self {
        Self::new(source)
    }
}
impl ArcSource {
    pub fn new(source: Source<'static>) -> Self {
        Self(Arc::new(source))
    }
}
impl Deref for ArcSource {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.source()
    }
}
impl AsRef<[u8]> for ArcSource {
    #[inline(always)]
    fn as_ref(&self) -> &[u8] {
        self.0.source_bytes()
    }
}
impl AsRef<str> for ArcSource {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.0.source()
    }
}
impl SourceFile for ArcSource {
    fn span(&self) -> SourceSpan {
        self.0.span()
    }
    fn source(&self) -> &str {
        self.0.source()
    }
}
impl NamedSourceFile for ArcSource {
    fn name(&self) -> FileName {
        self.0.name()
    }
}
impl SourceCode for ArcSource {
    #[inline(always)]
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        self.0
            .read_span(span, context_lines_before, context_lines_after)
    }
}

#[derive(Debug)]
pub struct Source<'a> {
    pub name: FileName,
    pub code: Cow<'a, str>,
}
impl<'a> From<&'a str> for Source<'a> {
    fn from(s: &'a str) -> Self {
        Self {
            name: FileName::Stdin,
            code: Cow::Borrowed(s),
        }
    }
}
impl<'a> From<String> for Source<'a> {
    fn from(code: String) -> Self {
        Self {
            name: FileName::Stdin,
            code: Cow::Owned(code),
        }
    }
}
impl<'a> Source<'a> {
    pub fn new<N, S>(name: N, code: S) -> Self
    where
        FileName: From<N>,
        Cow<'a, str>: From<S>,
    {
        Self {
            name: FileName::from(name),
            code: Cow::from(code),
        }
    }
}
impl Source<'static> {
    pub fn into_arc_source(self) -> ArcSource {
        ArcSource::new(self)
    }
}
impl<'a> SourceFile for Source<'a> {
    fn span(&self) -> SourceSpan {
        self.code.span()
    }
    fn source(&self) -> &str {
        self.code.source()
    }
}
impl<'a> NamedSourceFile for Source<'a> {
    fn name(&self) -> FileName {
        self.name.clone()
    }
}
impl<'s> SourceCode for Source<'s> {
    #[inline(always)]
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        self.code
            .read_span(span, context_lines_before, context_lines_after)
    }
}

pub trait Spanned {
    fn start(&self) -> usize {
        self.span().start()
    }
    fn end(&self) -> usize {
        self.span().end()
    }
    fn range(&self) -> Range<usize> {
        self.span().range()
    }
    fn span(&self) -> SourceSpan;
}
impl Spanned for SourceSpan {
    #[inline(always)]
    fn start(&self) -> usize {
        self.offset()
    }
    #[inline(always)]
    fn end(&self) -> usize {
        self.offset() + self.len()
    }
    #[inline]
    fn range(&self) -> Range<usize> {
        let offset = self.offset();
        Range::new(offset, offset + self.len())
    }
    fn span(&self) -> SourceSpan {
        *self
    }
}

/// Represents the source span of an item of type [T]
pub struct Span<T> {
    span: Range<usize>,
    spanned: T,
}
impl<T: Copy> Copy for Span<T> {}
impl<T: Clone> Clone for Span<T> {
    fn clone(&self) -> Self {
        Self {
            span: self.span,
            spanned: self.spanned.clone(),
        }
    }
}
impl<T> Span<T> {
    /// Create a span for `spanned` with `range`
    #[inline]
    pub fn new<R>(range: R, spanned: T) -> Self
    where
        Range<usize>: From<R>,
    {
        Self {
            span: Range::from(range),
            spanned,
        }
    }

    /// Create a span for `spanned` representing a single location, `offset`
    #[inline]
    pub fn at(offset: usize, spanned: T) -> Self {
        Self {
            span: Range::new(offset, offset),
            spanned,
        }
    }

    #[inline]
    pub fn map<U, F>(self, mut f: F) -> Span<U>
    where
        F: FnMut(T) -> U,
    {
        Span {
            span: self.span,
            spanned: f(self.spanned),
        }
    }

    /// Shift the span right by `count` units
    #[inline]
    pub fn shift(&mut self, count: usize) {
        self.span.start += count;
        self.span.end += count;
    }

    /// Extend the end of the span by `count` units
    #[inline]
    pub fn extend(&mut self, count: usize) {
        self.span.end += count;
    }

    #[inline]
    pub fn into_parts(self) -> (SourceSpan, T) {
        (self.span.into(), self.spanned)
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.spanned
    }
}
impl<T: Borrow<str>, S: Borrow<T>> Borrow<T> for Span<S> {
    fn borrow(&self) -> &T {
        self.spanned.borrow()
    }
}
impl<T> Deref for Span<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.spanned
    }
}
impl<T> DerefMut for Span<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.spanned
    }
}
impl<T: ?Sized, U: AsRef<T>> AsRef<T> for Span<U> {
    fn as_ref(&self) -> &T {
        self.spanned.as_ref()
    }
}
impl<T: ?Sized, U: AsMut<T>> AsMut<T> for Span<U> {
    fn as_mut(&mut self) -> &mut T {
        self.spanned.as_mut()
    }
}
impl<T: fmt::Debug> fmt::Debug for Span<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.spanned, f)
    }
}
impl<T: fmt::Display> fmt::Display for Span<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.spanned, f)
    }
}
impl<T: Eq> Eq for Span<T> {}
impl<T: PartialEq> PartialEq for Span<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.spanned.eq(&other.spanned)
    }
}
impl<T: PartialEq> PartialEq<T> for Span<T> {
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.spanned.eq(other)
    }
}
impl<T: Ord> Ord for Span<T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.spanned.cmp(&other.spanned)
    }
}
impl<T: PartialOrd> PartialOrd for Span<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        self.spanned.partial_cmp(&other.spanned)
    }
}
impl<T: Hash> Hash for Span<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.spanned.hash(state);
    }
}
impl<T> Spanned for Span<T> {
    #[inline(always)]
    fn start(&self) -> usize {
        self.span.start
    }

    #[inline(always)]
    fn end(&self) -> usize {
        self.span.end
    }

    #[inline(always)]
    fn range(&self) -> Range<usize> {
        self.span
    }

    #[inline]
    fn span(&self) -> SourceSpan {
        self.span.into()
    }
}
