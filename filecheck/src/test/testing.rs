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
    input_file: Option<ArcSource>,
    match_file: Option<ArcSource>,
}
impl Default for TestContext {
    fn default() -> Self {
        let result = diagnostics::reporting::set_hook(Box::new(|_| {
            Box::new(diagnostics::reporting::ReportHandlerOpts::new().build())
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

    pub fn with_checks<S>(&mut self, match_file: S) -> &mut Self
    where
        ArcSource: From<S>,
    {
        self.match_file = Some(ArcSource::from(match_file));
        self
    }

    pub fn with_input<S>(&mut self, input_file: S) -> &mut Self
    where
        ArcSource: From<S>,
    {
        self.input_file = Some(ArcSource::from(input_file));
        self
    }

    #[track_caller]
    pub fn match_file(&self) -> ArcSource {
        self.match_file.clone().expect("no match file was set")
    }

    #[track_caller]
    pub fn input_file(&self) -> ArcSource {
        self.input_file.clone().expect("no input file was set")
    }

    #[track_caller]
    pub fn check(&self) -> DiagResult<Vec<MatchInfo<'static>>> {
        self.check_input(self.input_file())
    }

    #[track_caller]
    pub fn check_failed(&self) -> TestFailed {
        self.check_input(self.input_file())
            .unwrap_err()
            .downcast::<TestFailed>()
            .unwrap()
    }

    #[track_caller]
    pub fn check_input<S>(&self, input_file: S) -> DiagResult<Vec<MatchInfo<'static>>>
    where
        ArcSource: From<S>,
    {
        let match_file = self.match_file();
        let input_file = ArcSource::from(input_file);
        self.check_with_match_and_input(match_file, input_file)
    }

    #[track_caller]
    pub fn check_with_match_and_input(
        &self,
        match_file: ArcSource,
        input_file: ArcSource,
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
            self.input_file.as_ref().unwrap().source_bytes(),
        )
    }

    #[track_caller]
    pub fn lex<'a, S: SourceFile + ?Sized>(&self, source: &'a S) -> Tokens<'a, S> {
        Tokens {
            source,
            lexer: Lexer::<'a>::new(
                source,
                &self.config.check_prefixes,
                &self.config.comment_prefixes,
            ),
        }
    }

    #[track_caller]
    pub fn lex_with_errors<'a, S: SourceFile + ?Sized>(
        &self,
        source: &'a S,
    ) -> TokensWithErrors<'a> {
        TokensWithErrors {
            lexer: Lexer::<'a>::new(
                source,
                &self.config.check_prefixes,
                &self.config.comment_prefixes,
            ),
        }
    }

    #[track_caller]
    pub fn parse<'a>(&mut self, source: &'a str) -> DiagResult<CheckFile<'a>> {
        let mut parser = parse::CheckFileParser::new(
            &self.config.check_prefixes,
            &self.config.comment_prefixes,
            &mut self.interner,
        );
        parser
            .parse(source)
            .map_err(|err| Report::new(err).with_source_code(source.to_string()))
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
impl<'a, S: SourceFile + ?Sized> Tokens<'a, S> {
    #[inline]
    pub fn next(&mut self) -> DiagResult<Option<Token<'a>>> {
        self.lexer
            .next()
            .transpose()
            .map_err(|err| Report::new(err).with_source_code(self.source.source().to_string()))
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
