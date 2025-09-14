//! Formats strings by expanding variables, similar to shell expansion.
//!
//! This crate provides a simple and efficient way to substitute variables in a
//! string, using either the process environment or a custom context like a
//! `HashMap`.
//!
//! The main entry points are the [`format()`] and [`format_with()`] functions.
//!
//! ## Syntax
//!
//! The formatting syntax is designed to be familiar to users of Unix shells.
//!
//! - `$VAR` Simple variables
//!   - A variable name starts with an alphabetic character or an underscore,
//!     followed by any number of alphanumeric characters or underscores.
//!   - The expansion is greedy, meaning it will match the longest possible
//!     valid variable name.
//!
//! - `${VAR}` Braced variables
//!   - This syntax is useful for separating a variable name from subsequent
//!     characters.
//!
//! - `${VAR:-default}` Default values
//!   - If `VAR` is not found in the context, the `default` value is used
//!     instead.
//!   - If `VAR` is present in the context, even if its value is an empty
//!     string, the default is **not** used. This matches standard shell
//!     behavior.
//!
//! - `$$` Escaping
//!   - To include a literal dollar sign in the output, use `$$`.
//!
//! ## Examples
//!
//! Using environment variables:
//!
//! ```rust
//! let formatted = envfmt::format("This package is $CARGO_PKG_NAME.").unwrap();
//! assert_eq!(formatted, "This package is envfmt.");
//! ```
//!
//! Using a custom context:
//!
//! ```
//! use std::collections::HashMap;
//!
//! let mut context = HashMap::new();
//! context.insert("thing", "world");
//!
//! let input = "Hello, ${thing}!";
//! let result = envfmt::format_with(input, &context).unwrap();
//!
//! assert_eq!(result, "Hello, world!");
//! ```

use std::{
    borrow::Borrow,
    collections::HashMap,
    env,
    hash::Hash,
    iter::Peekable,
};

use thiserror::Error;

/// Represents errors that can occur during formatting.
#[derive(Debug, Error, PartialEq)]
pub enum Error {
    /// A required variable was not found in the context.
    #[error("variable not found: '{0}'")]
    VariableNotFound(String),

    /// A variable name was invalid, e.g `${}`.
    #[error("invalid variable name: '{0}'")]
    InvalidVariableName(String),

    /// The input string have an unclosed brace.
    #[error("unexpected end of input: missing closing brace '}}'")]
    UnclosedBrace,
}

/// A trait for providing values for variable expansion.
///
/// This allows [`format_with`] to be generic over the source of the
/// variables, making it easy to test with a `HashMap` or use with environment
/// variables.
pub trait Context {
    /// Retrieves a value for a given key.
    ///
    /// # Parameters
    /// - `key`: The name of the variable to look up.
    ///
    /// # Returns
    /// - `Some(String)` if the key exists.
    /// - `None` if the key does not exist.
    fn get(&self, key: &str) -> Option<String>;
}

impl<K, S> Context for HashMap<K, S>
where
    K: Borrow<str> + Eq + Hash,
    S: AsRef<str>,
{
    fn get(&self, key: &str) -> Option<String> {
        self.get(key).map(|s| s.as_ref().to_string())
    }
}

// A `Context` implementation that reads from the process environment variables.
struct Env;

impl Context for Env {
    fn get(&self, key: &str) -> Option<String> {
        env::var(key).ok()
    }
}

/// Formats a string by expanding variables from a given context.
///
/// This is the generic version of the formatting function, which accepts any
/// type that implements the [`Context`] trait as the source for variable
/// values.
///
/// # Parameters
/// - `input`: The string template to format.
/// - `context`: A reference to a context that provides variable values.
///
/// # Returns
/// - `Ok(String)` with the formatted string if successful.
/// - `Err(Error)` if a variable is not found or the syntax is invalid.
///
/// # Examples
///
/// ```
/// use std::collections::HashMap;
/// use envfmt::{format_with, Context, Error};
///
/// let mut context = HashMap::new();
/// context.insert("VAR", "value");
///
/// // Successful expansion
/// assert_eq!(format_with("Hello, $VAR", &context).unwrap(), "Hello, value");
///
/// // Variable not found
/// assert_eq!(
///     format_with("Hello, $MISSING", &context).unwrap_err(),
///     Error::VariableNotFound("MISSING".to_string())
/// );
/// ```
pub fn format_with<C: Context>(
    input: &str,
    context: &C,
) -> Result<String, Error> {
    let mut result = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '$' {
            if let Some(next_char) = chars.peek() {
                match next_char {
                    // Escaped dollar sign: $$
                    '$' => {
                        result.push('$');
                        chars.next();
                    }
                    // Braced variable: ${VAR} or ${VAR:-default}
                    '{' => {
                        chars.next(); // Consume the '{'
                        format_braced_var(&mut chars, &mut result, context)?;
                    }
                    // Simple variable: $VAR
                    _ if next_char.is_alphabetic() || *next_char == '_' => {
                        format_var(&mut chars, &mut result, context)?;
                    }
                    // Just a literal dollar sign
                    _ => result.push('$'),
                }
            } else {
                result.push('$');
            }
        } else {
            result.push(c);
        }
    }

    Ok(result)
}

// Expands $VAR
fn format_var<C: Context>(
    chars: &mut Peekable<impl Iterator<Item = char>>,
    result: &mut String,
    context: &C,
) -> Result<(), Error> {
    let mut var_name = String::new();
    while let Some(c) = chars.peek() {
        if c.is_alphanumeric() || *c == '_' {
            var_name.push(*c);
            chars.next();
        } else {
            break;
        }
    }

    if var_name.is_empty() {
        // This case should theoretically not be hit due to the entry condition,
        // but as a safeguard.
        return Err(Error::InvalidVariableName("".to_string()));
    }

    match context.get(&var_name) {
        Some(value) => result.push_str(&value),
        None => return Err(Error::VariableNotFound(var_name)),
    }

    Ok(())
}

// Expands ${VAR}
fn format_braced_var<C: Context>(
    chars: &mut Peekable<impl Iterator<Item = char>>,
    result: &mut String,
    context: &C,
) -> Result<(), Error> {
    let mut var_name = String::new();

    // Parse the variable name part
    while let Some(c) = chars.peek() {
        match c {
            '}' => {
                chars.next();
                if var_name.is_empty() {
                    return Err(Error::InvalidVariableName("".to_string()));
                }
                return match context.get(&var_name) {
                    Some(value) => {
                        result.push_str(&value);
                        Ok(())
                    }
                    None => Err(Error::VariableNotFound(var_name)),
                };
            }
            ':' => {
                chars.next();
                if chars.peek() == Some(&'-') {
                    chars.next();
                    return resolve_default_value(
                        chars, result, &var_name, context,
                    );
                } else {
                    var_name.push(':');
                }
            }
            _ => {
                var_name.push(*c);
                chars.next();
            }
        }
    }

    // If we exit the loop, it means we ran out of characters before finding '}'
    Err(Error::UnclosedBrace)
}

fn resolve_default_value<C: Context>(
    chars: &mut Peekable<impl Iterator<Item = char>>,
    result: &mut String,
    var_name: &str,
    context: &C,
) -> Result<(), Error> {
    if var_name.is_empty() {
        return Err(Error::InvalidVariableName("".to_string()));
    }

    // Check context for the variable first. If it exists, use its value.
    if let Some(value) = context.get(var_name) {
        let mut brace_level = 0;
        // We need to discard the default value part, so we consume until the
        // matching '}'
        while let Some(c) = chars.next() {
            match c {
                '{' => brace_level += 1,
                '}' => {
                    if brace_level == 0 {
                        result.push_str(&value);
                        return Ok(());
                    }
                    brace_level -= 1;
                }
                _ => {}
            }
        }
        return Err(Error::UnclosedBrace);
    }

    // If variable is not in context, use the default value.
    let mut default_value = String::new();
    let mut brace_level = 0;
    while let Some(c) = chars.next() {
        match c {
            '{' => {
                brace_level += 1;
                default_value.push(c);
            }
            '}' => {
                if brace_level == 0 {
                    result.push_str(&default_value);
                    return Ok(());
                }
                brace_level -= 1;
                default_value.push(c);
            }
            _ => default_value.push(c),
        }
    }

    Err(Error::UnclosedBrace)
}

/// Formats a string by expanding variables from the process environment.
///
/// This is a convenience wrapper around [`format_with`] that uses
/// `std::env::vars` as the context.
///
/// # Examples
///
/// ```
/// let formatted = envfmt::format("This package is $CARGO_PKG_NAME.").unwrap();
/// assert_eq!(formatted, "This package is envfmt.");
/// ```
pub fn format(input: &str) -> Result<String, Error> {
    format_with(input, &Env)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_context() -> HashMap<String, String> {
        let mut ctx = HashMap::new();
        ctx.insert("VAR1".to_string(), "value1".to_string());
        ctx.insert("VAR2".to_string(), "value2".to_string());
        ctx.insert("EMPTY".to_string(), "".to_string());
        ctx
    }

    #[test]
    fn no_vars() {
        let ctx = create_context();
        assert_eq!(format_with("hello world", &ctx).unwrap(), "hello world");
    }

    #[test]
    fn var_simple() {
        let ctx = create_context();
        assert_eq!(format_with("hello $VAR1", &ctx).unwrap(), "hello value1");
    }

    #[test]
    fn var_braced() {
        let ctx = create_context();
        assert_eq!(format_with("hello ${VAR1}", &ctx).unwrap(), "hello value1");
    }

    #[test]
    fn var_multiple() {
        let ctx = create_context();
        assert_eq!(format_with("$VAR1-$VAR2", &ctx).unwrap(), "value1-value2");
        assert_eq!(
            format_with("${VAR1}-${VAR2}", &ctx).unwrap(),
            "value1-value2"
        );
    }

    #[test]
    fn var_adjacent() {
        let ctx = create_context();
        assert_eq!(format_with("$VAR1$VAR2", &ctx).unwrap(), "value1value2");
        assert_eq!(
            format_with("${VAR1}${VAR2}", &ctx).unwrap(),
            "value1value2"
        );
    }

    #[test]
    fn var_not_found_simple() {
        let ctx = create_context();
        assert_eq!(
            format_with("$NOT_FOUND", &ctx).unwrap_err(),
            Error::VariableNotFound("NOT_FOUND".to_string())
        );
    }

    #[test]
    fn var_not_found_braced() {
        let ctx = create_context();
        assert_eq!(
            format_with("${NOT_FOUND}", &ctx).unwrap_err(),
            Error::VariableNotFound("NOT_FOUND".to_string())
        );
    }

    #[test]
    fn var_invalid() {
        let ctx = create_context();
        assert_eq!(
            format_with("test ${}", &ctx).unwrap_err(),
            Error::InvalidVariableName("".to_string())
        );
        assert_eq!(
            format_with("test ${:-default}", &ctx).unwrap_err(),
            Error::InvalidVariableName("".to_string())
        );
    }

    #[test]
    fn default_value() {
        let ctx = create_context();
        assert_eq!(
            format_with("val: ${UNSET:-default_val}", &ctx).unwrap(),
            "val: default_val"
        );
    }

    #[test]
    fn default_value_ignored() {
        let ctx = create_context();
        assert_eq!(
            format_with("val: ${VAR1:-default_val}", &ctx).unwrap(),
            "val: value1"
        );
    }

    #[test]
    fn default_value_empty_var() {
        // NOTE: Standard shell behavior is to use the empty value if set.
        let ctx = create_context();
        assert_eq!(
            format_with("val: ${EMPTY:-default_val}", &ctx).unwrap(),
            "val: "
        );
    }

    #[test]
    fn default_value_with_braces() {
        let ctx = create_context();
        let input = "${UNSET:-{key: value}}";
        let expected = "{key: value}";
        assert_eq!(format_with(input, &ctx).unwrap(), expected);
    }

    #[test]
    fn default_value_unset() {
        let ctx = create_context();
        assert_eq!(format_with("val: ${UNSET:-}", &ctx).unwrap(), "val: ");
    }

    #[test]
    fn dollar_escaped() {
        let ctx = create_context();
        assert_eq!(
            format_with("this is not a $VAR1, it is $$VAR1", &ctx).unwrap(),
            "this is not a value1, it is $VAR1"
        );
        assert_eq!(format_with("$$", &ctx).unwrap(), "$");
    }

    #[test]
    fn dollar_suffix() {
        let ctx = create_context();
        assert_eq!(format_with("hello $", &ctx).unwrap(), "hello $");
    }

    #[test]
    fn dollar_with_space() {
        let ctx = create_context();
        assert_eq!(
            format_with("hello $ world", &ctx).unwrap(),
            "hello $ world"
        );
    }

    #[test]
    fn brace_unmatched() {
        let ctx = create_context();
        assert_eq!(
            format_with("test ${VAR1", &ctx).unwrap_err(),
            Error::UnclosedBrace
        );
        assert_eq!(
            format_with("test ${UNSET:-default", &ctx).unwrap_err(),
            Error::UnclosedBrace
        );
    }

    #[test]
    fn complex_string() {
        let ctx = create_context();
        let input = "path is $VAR1, version is ${VAR2:-1.0}, but not $UNDEFINED_VAR. Cost is $$50.";
        let expected_err = Error::VariableNotFound("UNDEFINED_VAR".to_string());
        assert_eq!(format_with(input, &ctx).unwrap_err(), expected_err);

        let input_with_default = "path is $VAR1, version is ${VAR2:-1.0}, maybe ${UNDEFINED_VAR:-fallback}. Cost is $$50.";
        let expected_str =
            "path is value1, version is value2, maybe fallback. Cost is $50.";
        assert_eq!(
            format_with(input_with_default, &ctx).unwrap(),
            expected_str
        );
    }
}
