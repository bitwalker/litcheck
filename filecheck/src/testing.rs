use litcheck::{
    diagnostics::{self, ArcSource, DiagResult, Report, SourceFile},
    StringInterner,
};

use crate::{
    check::{CheckFile, CheckProgram, FileCheckTest, MatchContext, TestFailed},
    parse::{self, Lexer, ParserError, Token},
    Config,
};

pub struct TestContext {
    pub config: Config,
    pub interner: StringInterner,
    input_file: Option<ArcSource>,
    match_file: Option<ArcSource>,
}
impl TestContext {
    pub fn new() -> Self {
        let result = diagnostics::reporting::set_hook(Box::new(|_| {
            Box::new(diagnostics::reporting::ReportHandlerOpts::new().build())
        }));

        if result.is_ok() {
            diagnostics::set_panic_hook();
        }

        Self {
            config: Config {
                check_prefixes: vec!["CHECK".to_string().into_boxed_str()],
                comment_prefixes: vec![
                    "COM".to_string().into_boxed_str(),
                    "RUN".to_string().into_boxed_str(),
                ],
                allow_unused_prefixes: true,
                strict_whitespace: false,
                match_full_lines: false,
                ignore_case: false,
                implicit_check_not: vec![],
                dump_input: Default::default(),
                dump_input_filter: Default::default(),
                enable_var_scope: true,
                variables: vec![],
                verbose: 0,
                color: Default::default(),
            },
            interner: StringInterner::new(),
            input_file: None,
            match_file: None,
        }
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
    pub fn check(&self) -> DiagResult<()> {
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
    pub fn check_input<S>(&self, input_file: S) -> DiagResult<()>
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
    ) -> DiagResult<()> {
        let mut test = FileCheckTest::new(match_file, &self.config);
        test.check(input_file)
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

    #[allow(unused)]
    #[track_caller]
    pub fn compile<'a, 'context: 'a>(
        &'context mut self,
        source: &'a str,
    ) -> DiagResult<CheckProgram<'a>> {
        let file = self.parse(source)?;

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
