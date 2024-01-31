use crate::{common::*, errors::InvalidNumericCastError};

/// A value on the match context operand stack
#[derive(Debug)]
pub struct Operand<'input>(pub CaptureInfo<'input>);
impl<'input> Operand<'input> {
    #[inline]
    pub fn into_value(self) -> Value<'input> {
        self.0.value
    }

    pub fn as_value(&self) -> &Value<'input> {
        &self.0.value
    }

    pub fn as_numeric(
        &self,
        span: SourceSpan,
        format: NumberFormat,
        context: &dyn Context<'input, '_>,
    ) -> Result<Number, InvalidNumericCastError> {
        match self.as_value() {
            Value::Undef => Err(InvalidNumericCastError {
                // TODO
                span: None,
                kind: std::num::IntErrorKind::Empty,
                specific_span: None,
                match_file: context.match_file(),
            }),
            Value::Str(value) => {
                let value = match format {
                    NumberFormat::Unsigned { precision } => {
                        parse_number(
                            span,
                            value.trim(),
                            precision,
                            /*signed=*/ false,
                            context,
                        )?
                    }
                    NumberFormat::Signed { precision } => {
                        parse_number(
                            span,
                            value.trim(),
                            precision,
                            /*signed=*/ true,
                            context,
                        )?
                    }
                    NumberFormat::Hex {
                        require_prefix,
                        precision,
                    } => {
                        let value = if require_prefix {
                            if let Some(stripped) = value.trim().strip_prefix("0x") {
                                stripped
                            } else {
                                return Err(InvalidNumericCastError {
                                    span: Some(span),
                                    // TODO
                                    kind: std::num::IntErrorKind::InvalidDigit,
                                    specific_span: None,
                                    match_file: context.match_file(),
                                });
                            }
                        } else {
                            value.trim()
                        };
                        if value.len() > precision as usize {
                            return Err(InvalidNumericCastError {
                                span: Some(span),
                                kind: std::num::IntErrorKind::PosOverflow,
                                specific_span: None,
                                match_file: context.match_file(),
                            });
                        }

                        let trimmed = value.trim_start_matches('0');
                        if trimmed.is_empty() {
                            0
                        } else {
                            i64::from_str_radix(trimmed, 16).map_err(|err| {
                                InvalidNumericCastError {
                                    span: Some(span),
                                    kind: err.kind().clone(),
                                    specific_span: None,
                                    match_file: context.match_file(),
                                }
                            })?
                        }
                    }
                };
                Ok(Number {
                    span,
                    format,
                    value,
                })
            }
            Value::Num(Expr::Num(num)) => Ok(num.clone()),
            Value::Num(_) => unreachable!(),
        }
    }
}

fn parse_number(
    span: SourceSpan,
    value: &str,
    precision: u8,
    signed: bool,
    context: &dyn Context<'_, '_>,
) -> Result<i64, InvalidNumericCastError> {
    let precision = precision as usize;
    let value = if precision > 0 {
        if value.len() > precision {
            return Err(InvalidNumericCastError {
                span: Some(span),
                kind: std::num::IntErrorKind::PosOverflow,
                specific_span: None,
                match_file: context.match_file(),
            });
        }
        let value = value.trim_start_matches('0');
        if value.is_empty() {
            return Ok(0);
        }
        value
    } else {
        value
    };
    let value = value
        .parse::<i64>()
        .map_err(|err| InvalidNumericCastError {
            span: Some(span),
            kind: err.kind().clone(),
            specific_span: None,
            match_file: context.match_file(),
        })?;
    if !signed && value < 0 {
        return Err(InvalidNumericCastError {
            span: Some(span),
            kind: std::num::IntErrorKind::NegOverflow,
            specific_span: None,
            match_file: context.match_file(),
        });
    }
    Ok(value)
}
