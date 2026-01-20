use std::{
    borrow::{Borrow, Cow},
    collections::BTreeMap,
    fmt,
    str::FromStr,
};

use miette::Context;

use crate::{
    Symbol,
    diagnostics::{
        self, DiagResult, Diagnostic, Label, Report, SourceId, SourceSpan, Span, Spanned,
    },
    range::Range,
};

pub trait ValueParser {
    type Value<'a>;

    fn try_parse<'input>(s: Span<&'input str>) -> DiagResult<Self::Value<'input>>;
}

impl ValueParser for str {
    type Value<'a> = &'a str;

    #[inline(always)]
    fn try_parse<'input>(s: Span<&'input str>) -> DiagResult<Self::Value<'input>> {
        Ok(s.into_inner())
    }
}

impl ValueParser for String {
    type Value<'a> = String;

    #[inline(always)]
    fn try_parse<'input>(s: Span<&'input str>) -> DiagResult<Self::Value<'input>> {
        Ok(s.into_inner().to_string())
    }
}
impl ValueParser for Cow<'_, str> {
    type Value<'a> = Cow<'a, str>;

    #[inline(always)]
    fn try_parse<'input>(s: Span<&'input str>) -> DiagResult<Self::Value<'input>> {
        Ok(Cow::Borrowed(s.into_inner()))
    }
}
impl ValueParser for i64 {
    type Value<'a> = i64;

    #[inline(always)]
    fn try_parse<'input>(s: Span<&'input str>) -> DiagResult<Self::Value<'input>> {
        let (span, s) = s.into_parts();
        s.parse::<i64>().map_err(|err| {
            Report::new(diagnostics::Diag::new(format!("{err}")).with_label(Label::at(span)))
        })
    }
}

pub trait TypedVariable: Clone + Sized {
    type Key<'a>;
    type Value<'a>;
    type Variable<'a>: Clone + Sized;

    fn try_parse<'input>(input: Span<&'input str>) -> Result<Self::Variable<'input>, Report>;
}

#[derive(Diagnostic, Debug)]
pub enum VariableError {
    #[diagnostic()]
    Empty(#[label] SourceSpan),
    #[diagnostic()]
    EmptyName(#[label] SourceSpan),
    #[diagnostic()]
    MissingEquals(#[label] SourceSpan),
}
impl VariableError {
    pub fn into_report(self) -> Report {
        Report::from(self)
    }
}
impl std::error::Error for VariableError {}
impl fmt::Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Empty(_) => f.write_str("invalid variable definition: expected expression of the form `NAME(=VALUE)?`"),
            Self::EmptyName(_) => f.write_str("invalid variable definition: name cannot be empty"),
            Self::MissingEquals(_) => f.write_str(
                "invalid variable definition: expected 'NAME=VALUE', but no '=' was found in the input",
            ),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum VariableName {
    Pseudo(Span<Symbol>),
    Global(Span<Symbol>),
    User(Span<Symbol>),
}

impl ValueParser for VariableName {
    type Value<'a> = VariableName;

    fn try_parse(input: Span<&str>) -> DiagResult<Self::Value<'_>> {
        let (span, s) = input.into_parts();
        let (prefix, unprefixed) = if let Some(name) = s.strip_prefix('$') {
            (Some('$'), name)
        } else if let Some(name) = s.strip_prefix('@') {
            (Some('@'), name)
        } else {
            (None, s)
        };
        if !is_valid_variable_name(unprefixed) {
            let offset = prefix.is_some() as u32;
            let span = SourceSpan::new(
                span.source_id(),
                Range::new(span.start() + offset, span.end()),
            );
            return Err(miette::miette!(
                labels = vec![Label::at(span).into()],
                help = "must be non-empty, and match the pattern `[A-Za-z_][A-Za-z0-9_]*`",
                "invalid variable name"
            )
            .with_source_code(s.to_string()));
        }

        let name = Symbol::intern(unprefixed);
        match prefix {
            None => Ok(Self::User(Span::new(span, name))),
            Some('$') => Ok(Self::Global(Span::new(span, name))),
            Some(_) => Ok(Self::Pseudo(Span::new(span, name))),
        }
    }
}

impl Spanned for VariableName {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Pseudo(name) | Self::Global(name) | Self::User(name) => name.span(),
        }
    }
}

impl VariableName {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Pseudo(s) | Self::Global(s) | Self::User(s) => s.as_str(),
        }
    }

    pub fn into_inner(self) -> Symbol {
        match self {
            Self::User(s) | Self::Global(s) | Self::Pseudo(s) => s.into_inner(),
        }
    }

    pub fn to_global(self) -> Self {
        match self {
            global @ (Self::Global(_) | Self::Pseudo(_)) => global,
            Self::User(name) => Self::Global(name),
        }
    }
}

impl fmt::Display for VariableName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global(name) => write!(f, "${name}"),
            Self::Pseudo(name) => write!(f, "@{name}"),
            Self::User(name) => f.write_str(name.as_str()),
        }
    }
}

impl<T> Borrow<T> for VariableName
where
    Symbol: Borrow<T>,
{
    fn borrow(&self) -> &T {
        match self {
            Self::Pseudo(s) | Self::Global(s) | Self::User(s) => s.inner().borrow(),
        }
    }
}

impl<T: ?Sized> AsRef<T> for VariableName
where
    Symbol: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        match self {
            Self::Pseudo(s) | Self::Global(s) | Self::User(s) => (**s).as_ref(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Variable<V> {
    pub name: VariableName,
    pub value: V,
}
impl<V> Clone for Variable<V>
where
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            name: self.name,
            value: self.value.clone(),
        }
    }
}

unsafe impl<V: Send> Send for Variable<V> {}

unsafe impl<V: Sync> Sync for Variable<V> {}

impl<V> Variable<V> {
    pub fn new<T>(name: VariableName, value: T) -> Self
    where
        V: From<T>,
    {
        Self {
            name,
            value: V::from(value),
        }
    }

    pub fn name(&self) -> &VariableName {
        &self.name
    }

    pub fn is_pseudo(&self) -> bool {
        matches!(self.name, VariableName::Pseudo(_))
    }

    pub fn is_global(&self) -> bool {
        matches!(self.name, VariableName::Global(_) | VariableName::Pseudo(_))
    }
}

impl<V> TypedVariable for Variable<V>
where
    V: FromStr + Clone,
    <V as FromStr>::Err: Diagnostic + Send + Sync + 'static,
{
    type Key<'a> = VariableName;
    type Value<'a> = V;
    type Variable<'a> = Variable<V>;

    fn try_parse<'input>(input: Span<&'input str>) -> Result<Self::Variable<'input>, Report> {
        let (span, s) = input.into_parts();
        if s.is_empty() {
            Err(VariableError::Empty(span)
                .into_report()
                .with_source_code(s.to_string()))
        } else if let Some((k, v)) = s.split_once('=') {
            if k.is_empty() {
                return Err(VariableError::EmptyName(span)
                    .into_report()
                    .with_source_code(s.to_string()));
            }
            let key_len = k.len() as u32;
            let key_span = SourceSpan::new(span.source_id(), Range::new(0, key_len));
            if !is_valid_variable_name(k) {
                return Err(miette::miette!(
                    labels = vec![Label::at(key_span).into()],
                    help = "variable names must match the pattern `[A-Za-z_][A-Za-z0-9_]*`",
                    "name contains invalid characters",
                )
                .with_source_code(s.to_string()));
            }
            let k = <VariableName as ValueParser>::try_parse(Span::new(key_span, k))
                .wrap_err("invalid variable name")?;
            let v = v
                .parse::<V>()
                .map_err(|err| Report::from(err).with_source_code(v.to_string()))
                .wrap_err("invalid variable value")?;
            Ok(Self::new(k, v))
        } else {
            Err(VariableError::MissingEquals(span)
                .into_report()
                .with_source_code(s.to_string()))
        }
    }
}

impl<V> clap::builder::ValueParserFactory for Variable<V>
where
    V: FromStr + Send + Sync + Clone + 'static,
    <V as FromStr>::Err: Diagnostic + Send + Sync + Clone + 'static,
    for<'a> Variable<V>:
        TypedVariable<Key<'a> = VariableName, Value<'a> = V> + Send + Sync + Clone + 'static,
{
    type Parser = VariableParser<Variable<V>>;

    fn value_parser() -> Self::Parser {
        Default::default()
    }
}

#[derive(Copy, Debug)]
pub struct VariableParser<T>(core::marker::PhantomData<T>);

impl<T> Clone for VariableParser<T> {
    fn clone(&self) -> Self {
        Self(core::marker::PhantomData)
    }
}

unsafe impl<T: Send> Send for VariableParser<T> {}

unsafe impl<T: Sync> Sync for VariableParser<T> {}

impl<T> Default for VariableParser<T> {
    fn default() -> Self {
        Self(core::marker::PhantomData)
    }
}
impl<T, V> clap::builder::TypedValueParser for VariableParser<T>
where
    V: Send + Sync + Clone + 'static,
    for<'a> T: TypedVariable<Key<'a> = VariableName, Value<'a> = V, Variable<'a> = T>
        + Send
        + Sync
        + Clone
        + 'static,
{
    type Value = T;

    fn parse_ref(
        &self,
        _cmd: &clap::Command,
        _arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        use clap::error::{Error, ErrorKind};

        let raw = value
            .to_str()
            .ok_or_else(|| Error::new(ErrorKind::InvalidUtf8))?;

        let span = SourceSpan::new(SourceId::UNKNOWN, Range::new(0, raw.len() as u32));
        <T as TypedVariable>::try_parse(Span::new(span, raw)).map_err(|err| {
            let err = if err.source_code().is_none() {
                err.with_source_code(raw.to_string())
            } else {
                err
            };
            let diag = crate::reporting::PrintDiagnostic::new(err);
            Error::raw(ErrorKind::InvalidValue, format!("{diag}"))
        })
    }
}

pub struct Variables<V>(BTreeMap<VariableName, V>);

impl<V> FromIterator<Variable<V>> for Variables<V>
where
    V: TypedVariable,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Variable<V>>,
    {
        Self(iter.into_iter().map(|var| (var.name, var.value)).collect())
    }
}

impl<V: TypedVariable> Variables<V> {
    pub fn is_defined<Q>(&self, k: &Q) -> bool
    where
        Q: Ord + Eq,
        VariableName: Borrow<Q>,
    {
        self.0.contains_key(k)
    }

    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        Q: Ord + Eq,
        VariableName: Borrow<Q>,
    {
        self.0.get(k)
    }

    pub fn define(&mut self, k: impl Into<VariableName>, v: V) -> Option<V> {
        self.0.insert(k.into(), v)
    }

    pub fn delete<Q>(&mut self, k: &Q) -> Option<Variable<V>>
    where
        Q: Ord + Eq,
        VariableName: Borrow<Q>,
    {
        self.0.remove_entry(k).map(|(k, v)| Variable::new(k, v))
    }
}

pub fn is_valid_variable_name(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c == '_' || c.is_ascii_alphabetic() => {
            for c in chars {
                if c != '_' && !c.is_ascii_alphanumeric() {
                    return false;
                }
            }
        }
        Some(_) | None => return false,
    }

    true
}
