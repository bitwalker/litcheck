use std::{
    borrow::{Borrow, Cow},
    collections::BTreeMap,
    fmt,
};

use crate::diagnostics::{self, DiagResult, Diagnostic, Label, Report, SourceSpan, Span, Spanned};

pub trait ValueParser {
    type Value<'a>;

    fn try_parse(s: Span<&str>) -> DiagResult<Self::Value<'_>>;
}
impl ValueParser for str {
    type Value<'a> = &'a str;

    #[inline(always)]
    fn try_parse(s: Span<&str>) -> DiagResult<Self::Value<'_>> {
        Ok(s.into_inner())
    }
}
impl ValueParser for &'static str {
    type Value<'a> = &'static str;

    #[inline(always)]
    fn try_parse(s: Span<&str>) -> DiagResult<Self::Value<'_>> {
        Ok(Box::leak::<'static>(s.to_string().into_boxed_str()))
    }
}
impl ValueParser for String {
    type Value<'a> = String;

    #[inline(always)]
    fn try_parse(s: Span<&str>) -> DiagResult<Self::Value<'_>> {
        Ok(s.into_inner().to_string())
    }
}
impl<'b> ValueParser for Cow<'b, str> {
    type Value<'a> = Cow<'a, str>;

    #[inline(always)]
    fn try_parse(s: Span<&str>) -> DiagResult<Self::Value<'_>> {
        Ok(Cow::Borrowed(s.into_inner()))
    }
}
impl ValueParser for i64 {
    type Value<'a> = i64;

    #[inline(always)]
    fn try_parse(s: Span<&str>) -> DiagResult<Self::Value<'_>> {
        let (span, s) = s.into_parts();
        s.parse::<i64>().map_err(|err| {
            Report::new(diagnostics::Diag::new(format!("{err}")).with_label(Label::at(span)))
        })
    }
}

pub trait TypedVariable: Clone + Sized {
    type Key<'a>;
    type Value<'a>;

    fn try_parse(input: Span<&str>) -> Result<Self, VariableError>;
}

#[derive(Diagnostic, Debug)]
pub enum VariableError {
    #[diagnostic()]
    Empty(#[label] SourceSpan),
    #[diagnostic()]
    EmptyName(#[label] SourceSpan),
    #[diagnostic(transparent)]
    Name(Report),
    #[diagnostic(transparent)]
    Value(Report),
    #[diagnostic()]
    MissingEquals(#[label] SourceSpan),
    #[diagnostic(transparent)]
    Format(Report),
}
impl VariableError {
    pub fn into_report(self) -> Report {
        match self {
            Self::Name(report) | Self::Value(report) | Self::Format(report) => report,
            _ => Report::from(self),
        }
    }
}
impl std::error::Error for VariableError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use std::error::Error;

        match self {
            Self::Name(ref report) | Self::Value(ref report) | Self::Format(ref report) => {
                AsRef::<dyn Error>::as_ref(report).source()
            }
            _ => None,
        }
    }
}
impl fmt::Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Empty(_) => f.write_str("invalid variable definition: expected expression of the form `NAME(=VALUE)?`"),
            Self::EmptyName(_) => f.write_str("invalid variable definition: name cannot be empty"),
            Self::Name(_) => f.write_str("invalid variable name"),
            Self::Value(_) => f.write_str("invalid variable value"),
            Self::MissingEquals(_) => f.write_str(
                "invalid variable definition: expected 'NAME=VALUE', but no '=' was found in the input",
            ),
            Self::Format(_) => f.write_str("invalid variable definition"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum VariableName<S = String> {
    Pseudo(Span<S>),
    Global(Span<S>),
    User(Span<S>),
}
impl<S: Copy> Copy for VariableName<S> {}
impl<S> ValueParser for VariableName<S>
where
    for<'a> S: ValueParser<Value<'a> = S>,
    for<'a> <S as ValueParser>::Value<'a>: AsRef<str>,
{
    type Value<'a> = VariableName<<S as ValueParser>::Value<'a>>;

    fn try_parse(input: Span<&str>) -> DiagResult<Self::Value<'_>> {
        let (span, s) = input.into_parts();
        let len = s.len();
        let (prefix, unprefixed) = if let Some(name) = s.strip_prefix('$') {
            (Some('$'), name)
        } else if let Some(name) = s.strip_prefix('@') {
            (Some('@'), name)
        } else {
            (None, s)
        };
        if !is_valid_variable_name(unprefixed) {
            let offset = prefix.is_some() as usize;
            return Err(miette::miette!(
                labels = vec![Label::at(offset..len).into()],
                help = "must be non-empty, and match the pattern `[A-Za-z_][A-Za-z0-9_]*`",
                "invalid variable name"
            ));
        }

        let name = <S as ValueParser>::try_parse(Span::new(span, unprefixed))?;
        match prefix {
            None => Ok(Self::User(Span::new(span, name))),
            Some('$') => Ok(Self::Global(Span::new(span, name))),
            Some(_) => Ok(Self::Pseudo(Span::new(span, name))),
        }
    }
}
impl<S> Spanned for VariableName<S> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Pseudo(name) | Self::Global(name) | Self::User(name) => name.span(),
        }
    }
}
impl<S> VariableName<S> {
    #[inline]
    pub fn map<T, F>(self, f: F) -> VariableName<T>
    where
        F: FnMut(S) -> T,
    {
        match self {
            Self::User(s) => VariableName::User(s.map(f)),
            Self::Global(s) => VariableName::Global(s.map(f)),
            Self::Pseudo(s) => VariableName::Pseudo(s.map(f)),
        }
    }
}
impl VariableName<String> {
    pub fn as_string(&self) -> &String {
        match self {
            Self::Pseudo(ref s) | Self::Global(ref s) | Self::User(ref s) => s,
        }
    }
}
impl<S: AsRef<str>> VariableName<S> {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Pseudo(ref s) | Self::Global(ref s) | Self::User(ref s) => (**s).as_ref(),
        }
    }
}
impl<S> VariableName<S> {
    pub fn into_inner(self) -> S {
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
impl<T: Borrow<str>, S: Borrow<T>> Borrow<T> for VariableName<S> {
    fn borrow(&self) -> &T {
        match self {
            Self::Pseudo(ref s) | Self::Global(ref s) | Self::User(ref s) => s.borrow(),
        }
    }
}
impl<T: ?Sized, S: AsRef<T>> AsRef<T> for VariableName<S> {
    fn as_ref(&self) -> &T {
        match self {
            Self::Pseudo(ref s) | Self::Global(ref s) | Self::User(ref s) => (**s).as_ref(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Variable<K, V> {
    pub name: VariableName<K>,
    pub value: V,
}
impl<K, V> Clone for Variable<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            value: self.value.clone(),
        }
    }
}
unsafe impl<K: Send, V: Send> Send for Variable<K, V> {}
unsafe impl<K: Send, V: Sync> Sync for Variable<K, V> {}
impl<K, V> Variable<K, V> {
    pub fn new<T>(name: VariableName<K>, value: T) -> Self
    where
        V: From<T>,
    {
        Self {
            name,
            value: V::from(value),
        }
    }

    pub fn name(&self) -> &VariableName<K> {
        &self.name
    }

    pub fn is_pseudo(&self) -> bool {
        matches!(self.name, VariableName::Pseudo(_))
    }

    pub fn is_global(&self) -> bool {
        matches!(self.name, VariableName::Global(_) | VariableName::Pseudo(_))
    }
}
impl<K, V> TypedVariable for Variable<K, V>
where
    for<'a> VariableName<K>: ValueParser<Value<'a> = VariableName<K>> + AsRef<str> + Clone + 'a,
    for<'a> K: Clone + 'a,
    for<'a> V: ValueParser<Value<'a> = V> + Clone + 'a,
{
    type Key<'a> = K;
    type Value<'a> = V;

    fn try_parse(input: Span<&str>) -> Result<Self, VariableError> {
        let (span, s) = input.into_parts();
        let len = s.len();
        if s.is_empty() {
            Err(VariableError::Empty(span))
        } else if let Some((k, v)) = s.split_once('=') {
            if k.is_empty() {
                return Err(VariableError::EmptyName(span));
            }
            let key_len = k.len();
            let key_span = SourceSpan::from(0..key_len);
            if !is_valid_variable_name(k) {
                return Err(VariableError::Name(miette::miette!(
                    labels = vec![Label::at(key_span).into()],
                    help = "variable names must match the pattern `[A-Za-z_][A-Za-z0-9_]*`",
                    "name contains invalid characters",
                )));
            }
            let k = <VariableName<K> as ValueParser>::try_parse(Span::new(key_span, k))
                .map_err(VariableError::Name)?;
            let value_span = SourceSpan::from((key_len + 1)..len);
            let v = <V as ValueParser>::try_parse(Span::new(value_span, v))
                .map_err(VariableError::Value)?;
            Ok(Self::new(k, v))
        } else {
            Err(VariableError::MissingEquals(span))
        }
    }
}
impl<K, V> clap::builder::ValueParserFactory for Variable<K, V>
where
    V: ValueParser,
    K: Send + Sync + Clone,
    for<'a> <V as ValueParser>::Value<'a>: Send + Sync + Clone,
    for<'a> Variable<K, V>:
        TypedVariable<Key<'a> = K, Value<'a> = V> + Send + Sync + Clone + 'static,
{
    type Parser = VariableParser<Variable<K, V>>;

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
impl<T, K, V> clap::builder::TypedValueParser for VariableParser<T>
where
    K: Send + Sync + Clone + 'static,
    V: Send + Sync + Clone + 'static,
    for<'a> T: TypedVariable<Key<'a> = K, Value<'a> = V> + Send + Sync + Clone + 'static,
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

        let span = SourceSpan::from(0..raw.len());
        <T as TypedVariable>::try_parse(Span::new(span, raw)).map_err(|err| {
            let err = err.into_report().with_source_code(raw.to_string());
            let diag = diagnostics::reporting::PrintDiagnostic::new(err);
            Error::raw(ErrorKind::InvalidValue, format!("{diag}"))
        })
    }
}

pub struct Variables<K, V>(BTreeMap<VariableName<K>, V>)
where
    VariableName<K>: Eq + Ord;
impl<K, V> FromIterator<Variable<K, V>> for Variables<K, V>
where
    VariableName<K>: Eq + Ord,
    V: TypedVariable,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Variable<K, V>>,
    {
        Self(iter.into_iter().map(|var| (var.name, var.value)).collect())
    }
}
impl<K, V> Variables<K, V>
where
    VariableName<K>: Eq + Ord,
    V: TypedVariable,
{
    pub fn is_defined<Q>(&self, k: &Q) -> bool
    where
        Q: Ord + Eq,
        VariableName<K>: Borrow<Q>,
    {
        self.0.contains_key(k)
    }

    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        Q: Ord + Eq,
        VariableName<K>: Borrow<Q>,
    {
        self.0.get(k)
    }

    pub fn define(&mut self, k: impl Into<VariableName<K>>, v: V) -> Option<V> {
        self.0.insert(k.into(), v)
    }

    pub fn delete<Q>(&mut self, k: &Q) -> Option<Variable<K, V>>
    where
        Q: Ord + Eq,
        VariableName<K>: Borrow<Q>,
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
