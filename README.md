# envfmt

A lightweight Rust crate for expanding environment-style variables in strings,
similar to shell expansion.

This crate provides a simple and efficient way to substitute variables in a
string, using either the process environment or a custom context like a
`HashMap`.

## Features

- **Zero Dependencies**: `envfmt` is built with only the Rust standard library
  (plus `thiserror` for convenience).
- **Familiar Syntax**: Supports common shell variable expansion patterns.
  - Simple variables: `$VAR`
  - Braced variables: `${VAR}`
  - Default values for unset variables: `${VAR:-default}`
  - Escaping: `$$` to get a literal `$`
- **Flexible & Testable**: Use the primary `envfmt::format()` for environment
  variables, or `envfmt::format_with()` with a `HashMap` for easy testing and
  custom data sources.
- **Ergonomic**: Works with `HashMap<String, _>` and `HashMap<&str, _>` keys out
  of the box.

## Getting Started

Add `envfmt` to your project's dependencies:

```bash
cargo add envfmt
```

## Usage

This is the most common use case. First, ensure an environment variable is set
in your shell:

```shell
export ETHERSCAN_API_KEY="key"
```

Then, use `envfmt::format()` to expand it:

```rust
let key = envfmt::format("${ETHERSCAN_API_KEY}").unwrap();
assert_eq!(key, "key");
```

If a variable might be missing, you can provide a fallback default value:

```rust
let log_config = "${LOG_LEVEL:-INFO}";
let log_level = envfmt::format(log_config).unwrap();
assert_eq!(log_level, "INFO");
```

For advance usage, or if your variables come from a source other than the
environment, use `envfmt::format_with()`. This is fast, safe, and doesn't
require modifying the global environment.

```rust
let mut context = HashMap::new();
context.insert("user", "Alice");

let template = "Hello $user!";
let message = envfmt::format_with(template, &context)?;
assert_eq!(message, "Hello Alice!");
```

## License

This project is licensed under the MIT License.
