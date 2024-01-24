use std::path::Path;
use std::sync::Arc;

use miden_diagnostics::{CodeMap, DiagnosticsHandler};

use crate::command::RunConfig;
use crate::{check, FileCheckError, FileCheckRequest, FileCheckResult, Input};

pub struct TestResults {
    pub errors: Vec<check::CheckError>,
    pub succeeded: usize,
}

pub fn run_many(
    cwd: &Path,
    run: Option<String>,
    inputs: Vec<Input>,
    request: &FileCheckRequest,
) -> FileCheckResult<TestResults> {
    let run_config = match run {
        None => None,
        Some(arg) => Some(RunConfig::new(arg)?),
    };
    let codemap = Arc::new(CodeMap::new());
    let diagnostics = configure_diagnostics(codemap.clone());
    let mut succeeded = 0;
    let mut errors = vec![];
    for input in inputs.into_iter() {
        let result = run_once_with_config(
            cwd,
            run_config.as_ref(),
            input,
            request,
            codemap.clone(),
            diagnostics.clone(),
        );
        match result {
            Ok(_) => {
                succeeded += 1;
            }
            Err(FileCheckError::CheckFailed(err)) => {
                errors.push(err);
            }
            Err(err) => return Err(err),
        }
    }

    Ok(TestResults { errors, succeeded })
}

pub fn run_once(
    cwd: &Path,
    run: Option<String>,
    input: Input,
    request: &FileCheckRequest,
) -> FileCheckResult<TestResults> {
    let run_config = match run {
        None => None,
        Some(arg) => Some(RunConfig::new(arg)?),
    };
    let codemap = Arc::new(CodeMap::new());
    let diagnostics = configure_diagnostics(codemap.clone());
    let result = run_once_with_config(
        cwd,
        run_config.as_ref(),
        input,
        request,
        codemap,
        diagnostics,
    );

    match result {
        Ok(_) => Ok(TestResults {
            errors: vec![],
            succeeded: 1,
        }),
        Err(FileCheckError::CheckFailed(err)) => Ok(TestResults {
            errors: vec![err],
            succeeded: 0,
        }),
        Err(err) => Err(err),
    }
}

fn run_once_with_config(
    _cwd: &Path,
    _run: Option<&RunConfig>,
    input: Input,
    request: &FileCheckRequest,
    codemap: Arc<CodeMap>,
    diagnostics: Arc<DiagnosticsHandler>,
) -> FileCheckResult<()> {
    let test = check::FileCheckTest::new(input, codemap, diagnostics, request)?;
    test.check()
}

fn configure_diagnostics(codemap: Arc<CodeMap>) -> Arc<DiagnosticsHandler> {
    use miden_diagnostics::term::termcolor::ColorChoice;
    use miden_diagnostics::DefaultEmitter;

    let emitter = Arc::new(DefaultEmitter::new(ColorChoice::Auto));
    Arc::new(DiagnosticsHandler::new(
        Default::default(),
        codemap,
        emitter,
    ))
}
