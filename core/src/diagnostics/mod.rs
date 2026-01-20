mod filename;
mod location;
mod selection;
mod source_file;
mod source_manager;
mod span;

pub use serde_spanned;

pub use self::source_manager::SourceManagerExt;
pub use self::{
    filename::FileName,
    location::{FileLineCol, Location},
    selection::{Position, Selection},
    source_file::{
        ByteIndex, ByteOffset, ColumnIndex, ColumnNumber, LineIndex, LineNumber, SourceContent,
        SourceContentUpdateError, SourceFile, SourceFileRef, SourceLanguage,
    },
    source_manager::{
        DefaultSourceManager, SourceId, SourceManager, SourceManagerError, SourceManagerSync,
    },
    span::{SourceSpan, Span, Spanned},
};

pub use miette::{
    Diagnostic, IntoDiagnostic, LabeledSpan, Report, Severity, SourceCode, SourceOffset, WrapErr,
    bail, diagnostic,
};

#[cfg(feature = "fancy-diagnostics")]
pub use miette::set_panic_hook;

pub type Diag = miette::MietteDiagnostic;
pub type DiagResult<T> = miette::Result<T>;

use std::{borrow::Cow, hash::Hash};

use crate::StaticCow;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    span: SourceSpan,
    label: Option<StaticCow<str>>,
}

impl Label {
    pub fn at(span: SourceSpan) -> Self {
        Self { span, label: None }
    }

    pub fn point<L>(source_id: SourceId, offset: impl Into<ByteIndex>, label: L) -> Self
    where
        StaticCow<str>: From<L>,
    {
        Self {
            span: SourceSpan::at(source_id, offset),
            label: Some(Cow::from(label)),
        }
    }

    pub fn new<L>(span: SourceSpan, label: L) -> Self
    where
        StaticCow<str>: From<L>,
    {
        Self {
            span,
            label: Some(Cow::from(label)),
        }
    }

    pub fn label(&self) -> Option<&str> {
        self.label.as_deref()
    }
}

impl From<Label> for SourceSpan {
    #[inline(always)]
    fn from(label: Label) -> SourceSpan {
        label.span
    }
}

impl From<Label> for LabeledSpan {
    #[inline]
    fn from(label: Label) -> LabeledSpan {
        if let Some(message) = label.label {
            LabeledSpan::at(label.span, message)
        } else {
            LabeledSpan::underline(label.span)
        }
    }
}
