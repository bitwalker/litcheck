pub use miette::{
    DebugReportHandler, JSONReportHandler, NarratableReportHandler, ReportHandler, set_hook,
};

use crate::diagnostics::Diagnostic;

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
impl<D: AsRef<dyn Diagnostic>> PrintDiagnostic<D> {
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
impl<D: AsRef<dyn Diagnostic>> PrintDiagnostic<D, NarratableReportHandler> {
    pub fn narrated(diag: D) -> Self {
        Self {
            handler: NarratableReportHandler::default(),
            diag,
        }
    }
}
impl<D: AsRef<dyn Diagnostic>> PrintDiagnostic<D, JSONReportHandler> {
    pub fn json(diag: D) -> Self {
        Self {
            handler: JSONReportHandler,
            diag,
        }
    }
}
impl<D: AsRef<dyn Diagnostic>> core::fmt::Display for PrintDiagnostic<D> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.handler.render_report(f, self.diag.as_ref())
    }
}
impl<D: AsRef<dyn Diagnostic>> core::fmt::Display for PrintDiagnostic<D, NarratableReportHandler> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.handler.render_report(f, self.diag.as_ref())
    }
}
impl<D: AsRef<dyn Diagnostic>> core::fmt::Display for PrintDiagnostic<D, JSONReportHandler> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.handler.render_report(f, self.diag.as_ref())
    }
}
