use litcheck::diagnostics;

use crate::{
    ast::CheckFile,
    check::CheckProgram,
    common::*,
    parse::{self, Lexer, ParserError, Token},
    test::Test,
};

pub struct TestContext {
    pub config: Config,
    pub interner: StringInterner,
    input_file: Option<Arc<SourceFile>>,
    match_file: Option<Arc<SourceFile>>,
}
impl Default for TestContext {
    fn default() -> Self {
        let result = reporting::set_hook(Box::new(|_| {
            Box::new(reporting::ReportHandlerOpts::new().build())
        }));

        if result.is_ok() {
            diagnostics::set_panic_hook();
        }

        Self {
            config: Config::default(),
            interner: StringInterner::new(),
            input_file: None,
            match_file: None,
        }
    }
}
impl TestContext {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_checks(&mut self, match_file: Arc<SourceFile>) -> &mut Self {
        self.match_file = Some(match_file);
        self
    }

    pub fn with_input(&mut self, input_file: Arc<SourceFile>) -> &mut Self {
        self.input_file = Some(input_file);
        self
    }

    #[track_caller]
    pub fn match_file(&self) -> Arc<SourceFile> {
        self.match_file.clone().expect("no match file was set")
    }

    #[track_caller]
    pub fn input_file(&self) -> Arc<SourceFile> {
        self.input_file.clone().expect("no input file was set")
    }

    #[track_caller]
    pub fn check(&self) -> DiagResult<Vec<MatchInfo<'static>>> {
        let mut test = Test::new(self.match_file(), &self.config);
        test.verify(self.input_file())
    }

    #[track_caller]
    pub fn check_failed(&self) -> TestFailed {
        let match_file = self.match_file();
        let mut test = Test::new(match_file, &self.config);
        test.verify(self.input_file())
            .unwrap_err()
            .downcast::<TestFailed>()
            .unwrap()
    }

    #[track_caller]
    pub fn check_input(&self, input: Arc<SourceFile>) -> DiagResult<Vec<MatchInfo<'static>>> {
        let match_file = self.match_file();
        let mut test = Test::new(match_file, &self.config);
        test.verify(input)
    }

    #[track_caller]
    pub fn check_with_match_and_input(
        &self,
        match_file: Arc<SourceFile>,
        input_file: Arc<SourceFile>,
    ) -> DiagResult<Vec<MatchInfo<'static>>> {
        let mut test = Test::new(match_file, &self.config);
        test.verify(input_file)
    }

    #[track_caller]
    pub fn match_context(&mut self) -> MatchContext<'_, '_> {
        let input_file = self.input_file();
        let match_file = self.match_file();
        MatchContext::new(
            &self.config,
            &mut self.interner,
            match_file,
            input_file,
            self.input_file.as_ref().unwrap().as_bytes(),
        )
    }

    #[track_caller]
    pub fn lex<'a>(&self, source: &'a SourceFile) -> Tokens<'a, str> {
        Tokens {
            source: source.as_str(),
            lexer: Lexer::<'a>::new(source.id(), source.as_str(), &self.config),
        }
    }

    #[track_caller]
    pub fn lex_with_errors<'a>(&self, source: &'a SourceFile) -> TokensWithErrors<'a> {
        TokensWithErrors {
            lexer: Lexer::<'a>::new(source.id(), source.as_str(), &self.config),
        }
    }

    #[track_caller]
    pub fn parse_str<'a, 'ctx: 'a>(&'ctx mut self, source: &str) -> DiagResult<CheckFile<'a>> {
        let caller = core::panic::Location::caller();
        let test_name = format!("{}{}_checks", &caller.file(), &caller.line());
        let source_file = self.config.source_manager.load(
            SourceLanguage::Unknown,
            FileName::from(test_name),
            source.to_string(),
        );
        let mut parser = parse::CheckFileParser::new(&self.config, &mut self.interner);

        let source = unsafe { &*(source_file.as_ref() as *const SourceFile) };

        parser
            .parse(source.id(), source.as_str())
            .map_err(|err| Report::from(err).with_source_code(source_file))
    }

    #[track_caller]
    pub fn parse<'a>(&mut self, source: &'a Arc<SourceFile>) -> DiagResult<CheckFile<'a>> {
        let mut parser = parse::CheckFileParser::new(&self.config, &mut self.interner);
        parser
            .parse(source.id(), source.as_str())
            .map_err(|err| Report::from(err).with_source_code(source.clone()))
    }

    #[track_caller]
    pub fn parse_err<'a>(
        &mut self,
        source: &'a Arc<SourceFile>,
    ) -> Result<CheckFile<'a>, ParserError> {
        let mut parser = parse::CheckFileParser::new(&self.config, &mut self.interner);
        parser.parse(source.id(), source.as_str())
    }

    #[track_caller]
    pub fn compile<'a>(&mut self, file: CheckFile<'a>) -> DiagResult<CheckProgram<'a>> {
        file.compile(&self.config, &mut self.interner)
    }
}

pub struct Tokens<'a, S: ?Sized> {
    source: &'a S,
    lexer: Lexer<'a>,
}
impl<'a, S: ?Sized + AsRef<str>> Tokens<'a, S> {
    #[inline]
    pub fn next(&mut self) -> DiagResult<Option<Token<'a>>> {
        self.lexer
            .next()
            .transpose()
            .map_err(|err| Report::from(err).with_source_code(self.source.as_ref().to_string()))
            .map(|opt| opt.map(|(_, tok, _)| tok))
    }
}

pub struct TokensWithErrors<'a> {
    lexer: Lexer<'a>,
}
impl<'a> TokensWithErrors<'a> {
    #[inline]
    pub fn next(&mut self) -> Result<Option<Token<'a>>, ParserError> {
        self.lexer
            .next()
            .transpose()
            .map(|opt| opt.map(|(_, tok, _)| tok))
    }
}
