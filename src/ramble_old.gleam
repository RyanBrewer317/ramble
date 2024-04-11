// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/string
import gleam/list
import gleam/io
import gleam/int
import gleam/option.{type Option, None, Some}

pub type Position {
  Position(row: Int, col: Int)
}

pub type Token(t) {
  Token(t)
  LabeledToken(String)
}

pub type ParseError(t) {
  Unexpected(pos: Position, token: Token(t), expected: List(Token(t)))
}

pub type ParseResult(t, a) {
  ParseResult(
    value: Result(a, ParseError(t)),
    rest: List(t),
    new_pos: Position,
    errors: List(ParseError(t)),
  )
}

pub type FinalParseResult(t, a) {
  FinalParseResult(value: Result(a, ParseError(t)), errors: List(ParseError(t)))
}

pub type ParserConfig(t) {
  ParserConfig(
    newline: t,
    eof: t,
    source_name: String,
    token_to_string: fn(t) -> String,
  )
}

pub type Parser(t, a) {
  Parser(
    parse: fn(List(t), Position, ParserConfig(t), Option(Parser(t, t))) ->
      ParseResult(t, a),
  )
}

pub fn get_config() -> Parser(t, ParserConfig(t)) {
  Parser(fn(source, pos, config, _recovery) {
    ParseResult(value: Ok(config), rest: source, new_pos: pos, errors: [])
  })
}

fn run_on_tokens(
  p: Parser(t, a),
  src: List(t),
  pos: Position,
  config: ParserConfig(t),
  recovery: Option(Parser(t, t)),
) -> ParseResult(t, a) {
  p.parse(src, pos, config, recovery)
}

pub fn go_on_tokens(
  p: Parser(t, a),
  src: List(t),
  config: ParserConfig(t),
) -> FinalParseResult(t, a) {
  let res = run_on_tokens(p, src, Position(1, 1), config, None)
  FinalParseResult(value: res.value, errors: res.errors)
}

pub fn go(
  p: Parser(String, a),
  src: String,
  config: ParserConfig(String),
) -> FinalParseResult(String, a) {
  go_on_tokens(p, string.to_graphemes(src), config)
}

pub fn recover() -> Parser(t, Nil) {
  Parser(fn(source, pos, config, recovery) {
    let p = case recovery {
      Some(r) -> {
        until(until: r, do: any_token())
        |> map(fn(_) {Nil})
      }
      None -> {
        any_token()
        |> map(fn(_) {Nil})
      }
    }
    let res = run_on_tokens(p, source, pos, config, None)
    ParseResult(
      value: Ok(Nil),
      rest: res.rest,
      new_pos: res.new_pos,
      errors: [],
    )
  })
}

/// Get the current parser position.
pub fn position() -> Parser(t, Position) {
  Parser(fn(source, p, _config, _recovery) {
    ParseResult(value: Ok(p), rest: source, new_pos: p, errors: [])
  })
}

/// Parse a character if it matches the predicate.
pub fn satisfy(when pred: fn(t) -> Bool) -> Parser(t, t) {
  Parser(fn(source, pos, config, recovery) {
    let assert Position(row, col) = pos
    case source {
      [h, ..t] ->
        case pred(h) {
          True -> {
            let new_pos = case h == config.newline {
              True -> Position(row + 1, 0)
              False -> Position(row, col + 1)
            }
            ParseResult(value: Ok(h), rest: t, new_pos: new_pos, errors: [])
          }
          False -> {
            let err = Unexpected(pos, Token(h), [])
            let res = run_on_tokens(recover(), source, pos, config, recovery)
            io.debug(#(pos, res.new_pos, recovery))
            ParseResult(
              value: Error(err),
              rest: res.rest,
              new_pos: res.new_pos,
              errors: [],
            )
          }
        }
      [] -> {
        let err = Unexpected(pos, Token(config.eof), [])
        ParseResult(value: Error(err), rest: source, new_pos: pos, errors: [])
      }
    }
  })
}

pub fn any_token() -> Parser(t, t) {
  Parser(fn(source, pos, config, _recovery) {
    let assert Position(row, col) = pos
    case source {
      [h, ..t] -> {
        let new_pos = case h == config.newline {
          True -> Position(row + 1, 0)
          False -> Position(row, col + 1)
        }
        ParseResult(value: Ok(h), rest: t, new_pos: new_pos, errors: [])
      }
      [] -> {
        let err = Unexpected(pos, Token(config.eof), [])
        ParseResult(value: Error(err), rest: source, new_pos: pos, errors: [])
      }
    }
  })
}

/// Parse a lowercase letter.
pub fn lowercase_letter() -> Parser(String, String) {
  use <- label("a lowercase letter")
  // TODO: switch to codepoint interval check for better performance
  satisfy(when: string.contains("abcdefghijklmnopqrstuvwxyz", _))
}

/// Parse an uppercase letter.
pub fn uppercase_letter() -> Parser(String, String) {
  use <- label("an uppercase letter")
  // TODO: switch to codepoint interval check for better performance
  satisfy(when: string.contains("ABCDEFGHIJKLMNOPQRSTUVWXYZ", _))
}

/// Parse a lowercase or uppercase letter.
pub fn letter() -> Parser(String, String) {
  use <- label("a letter")
  // TODO: switch to codepoint interval check for better performance
  satisfy(when: string.contains(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    _,
  ))
}

/// Parse a specific token.
pub fn token(c: t) -> Parser(t, t) {
  use config <- extract(get_config())
  use <- label("a '" <> config.token_to_string(c) <> "'")
  satisfy(when: fn(c2) { c == c2 })
}

/// Parse a digit.
pub fn digit() -> Parser(String, String) {
  use <- label("a digit")
  // TODO: switch to codepoint interval check for better performance
  satisfy(when: string.contains("0123456789", _))
}

/// Parse a sequence of digits.
pub fn digits() -> Parser(String, String) {
  many1_as_string(digit())
}

/// Parse the first parser, or the second if the first fails.
pub fn either(p: Parser(t, a), q: Parser(t, a)) -> Parser(t, a) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p, source, pos, config, recovery)
    case res.value {
      Ok(_) -> res
      Error(e1) -> {
        let res2 = run_on_tokens(q, source, pos, config, recovery)
        case res2.value {
          Ok(_) -> res2
          Error(e2) -> {
            let err =
              Unexpected(pos, e2.token, list.append(e1.expected, e2.expected))
            ParseResult(
              value: Error(err),
              rest: source,
              new_pos: pos,
              errors: [],
            )
          }
        }
      }
    }
  })
}

/// Parse with the first parser in the list that doesn't fail.
pub fn choice(ps: List(Parser(t, a))) -> Parser(t, a) {
  choice_helper(ps, [])
}

fn choice_helper(
  ps: List(Parser(t, a)),
  expected: List(Token(t)),
) -> Parser(t, a) {
  Parser(fn(source, pos, config, recovery) {
    case ps {
      [] -> panic as "`ramble.choice()` doesn't accept an empty list of parsers"
      [p] -> {
        let res = run_on_tokens(p, source, pos, config, recovery)
        case res.value {
          Ok(_) -> res
          Error(e) -> {
            let err =
              Unexpected(
                e.pos,
                e.token,
                list.reverse(list.append(e.expected, expected)),
              )
            ParseResult(
              value: Error(err),
              rest: source,
              new_pos: pos,
              errors: [],
            )
          }
        }
      }
      [p, ..t] -> {
        let res = run_on_tokens(p, source, pos, config, recovery)
        case res.value {
          Ok(v) ->
            ParseResult(value: Ok(v), rest: source, new_pos: pos, errors: [])
          Error(e) ->
            run_on_tokens(
              choice_helper(t, list.append(e.expected, expected)),
              source,
              pos,
              config,
              recovery,
            )
        }
      }
    }
  })
}

/// Parse an alphanumeric character.
pub fn alphanum() -> Parser(String, String) {
  use <- label("an alphanumeric character")
  // TODO: switch to codepoint interval check for better performance
  satisfy(when: string.contains(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
    _,
  ))
}

const whitespace_chars = " \t\n"

/// Parse zero or more whitespace characters.
pub fn whitespace() -> Parser(String, String) {
  use <- label("whitespace")
  many_as_string(satisfy(when: string.contains(whitespace_chars, _)))
}

/// Parse one or more whitespace characters.
pub fn whitespace1() -> Parser(String, String) {
  use <- label("whitespace")
  many1_as_string(satisfy(when: string.contains(whitespace_chars, _)))
}

/// Keep trying the parser until it fails, and return the array of parsed results.
/// This cannot fail because it parses zero or more times!
pub fn many(p: Parser(t, a)) -> Parser(t, List(a)) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p, source, pos, config, recovery)
    case res.value {
      Error(_) ->
        ParseResult(value: Ok([]), rest: source, new_pos: pos, errors: [])
      Ok(v) -> {
        let res2 =
          run_on_tokens(many(p), res.rest, res.new_pos, config, recovery)
        case res2.value {
          Error(_) ->
            ParseResult(value: Ok([]), rest: source, new_pos: pos, errors: [])
          Ok(vs) -> {
            ParseResult(
              value: Ok([v, ..vs]),
              rest: res2.rest,
              new_pos: res2.new_pos,
              errors: list.append(res.errors, res2.errors),
            )
          }
        }
      }
    }
  })
}

/// Parse a certain string as many times as possible, returning everything that was parsed.
/// This cannot fail because it parses zero or more times!
pub fn many_as_string(p: Parser(t, String)) -> Parser(t, String) {
  many(p)
  |> map(string.concat)
}

/// Keep trying the parser until it fails, and return the array of parsed results.
/// This can fail, because it must parse successfully at least once!
pub fn many1(p: Parser(t, a)) -> Parser(t, List(a)) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p, source, pos, config, recovery)
    case res.value {
      Error(e) ->
        ParseResult(
          value: Error(e),
          rest: source,
          new_pos: pos,
          errors: res.errors,
        )
      Ok(v) -> {
        let res2 =
          run_on_tokens(many(p), res.rest, res.new_pos, config, recovery)
        case res2.value {
          Error(_) ->
            ParseResult(
              value: Ok([v]),
              rest: res.rest,
              new_pos: res.new_pos,
              errors: res.errors,
            )
          Ok(vs) ->
            ParseResult(
              value: Ok([v, ..vs]),
              rest: res2.rest,
              new_pos: res2.new_pos,
              errors: list.append(res.errors, res2.errors),
            )
        }
      }
    }
  })
}

/// Parse a certain string as many times as possible, returning everything that was parsed.
/// This can fail, because it must parse successfully at least once!
pub fn many1_as_string(p: Parser(t, String)) -> Parser(t, String) {
  many1(p)
  |> map(string.concat)
}

/// Do the first parser, ignore its result, then do the second parser.
pub fn seq(p: Parser(t, a), q: Parser(t, b)) -> Parser(t, b) {
  Parser(fn(source, pos, config, recovery) {
    // TODO: the expectations for this should be those of the first parser but 
    // for some reason they are those of the second parser
    // TODO: nvm the above, for some reason this function is super broken.
    let res1 = run_on_tokens(p, source, pos, config, recovery)
    let res1_errors = get_errors(res1)
    let res2 = run_on_tokens(q, res1.rest, res1.new_pos, config, recovery)
    ParseResult(
      res2.value,
      res2.rest,
      res2.new_pos,
      list.append(res1_errors, res2.errors),
    )
  })
}

pub fn get_errors(res: ParseResult(t, a)) -> List(ParseError(t)) {
  case res.value {
    Ok(_) -> res.errors
    Error(e) -> [e, ..res.errors]
  }
}

pub fn do_and_recover(p: Parser(t, a), recovery r: Parser(t, t)) -> Parser(t, a) {
  Parser(fn(source, pos, config, _recovery) {
    run_on_tokens(p, source, pos, config, Some(r))
  })
}

/// Parse a sequence separated by the given separator parser.
pub fn sep(parser: Parser(t, a), by s: Parser(t, b)) -> Parser(t, List(a)) {
  use res <- extract(optional(sep1(parser, by: s)))
  case res {
    Ok(sequence) -> return(sequence)
    Error(Nil) -> return([])
  }
}

/// Parse a sequence separated by the given separator parser.
/// This only succeeds if at least one element of the sequence was parsed.
pub fn sep1(parser: Parser(t, a), by s: Parser(t, b)) -> Parser(t, List(a)) {
  use first <- extract(parser)
  use rest <- extract(many(seq(s, parser)))
  return([first, ..rest])
}

/// Do `p`, then apply `f` to the result if it succeeded.
pub fn map(p: Parser(t, a), f: fn(a) -> b) -> Parser(t, b) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p, source, pos, config, recovery)
    case res.value {
      Ok(v) ->
        ParseResult(
          value: Ok(f(v)),
          rest: res.rest,
          new_pos: res.new_pos,
          errors: res.errors,
        )
      Error(e) ->
        ParseResult(
          value: Error(e),
          rest: source,
          new_pos: pos,
          errors: res.errors,
        )
    }
  })
}

/// Do `p`, the apply `f` to the result if it succeeded.
/// `f` itself can fail with the user-defined error type, 
/// and if it does the result is a `UserError` with the error.
pub fn try(
  p: Parser(t, a),
  f: fn(a) -> Result(a, ParseError(t)),
) -> Parser(t, a) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p, source, pos, config, recovery)
    case res.value {
      Ok(v) ->
        case f(v) {
          Ok(a) ->
            ParseResult(
              value: Ok(a),
              rest: res.rest,
              new_pos: res.new_pos,
              errors: res.errors,
            )
          Error(e) ->
            ParseResult(value: Error(e), rest: source, new_pos: pos, errors: [])
        }
      Error(e) ->
        ParseResult(
          value: Error(e),
          rest: source,
          new_pos: pos,
          errors: res.errors,
        )
    }
  })
}

/// Try running a parser, but still succeed (with `Error(Nil)`) if it failed.
pub fn optional(p: Parser(t, a)) -> Parser(t, Result(a, Nil)) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p, source, pos, config, recovery)
    case res.value {
      Ok(v) ->
        ParseResult(
          value: Ok(Ok(v)),
          rest: res.rest,
          new_pos: res.new_pos,
          errors: [],
        )
      Error(_) ->
        ParseResult(
          value: Ok(Error(Nil)),
          rest: source,
          new_pos: pos,
          errors: [],
        )
    }
  })
}

/// Do each parser in the list, returning the result of the last parser.
pub fn all(ps: List(Parser(t, a))) -> Parser(t, a) {
  case ps {
    [p] -> p
    [h, ..t] -> {
      use _ <- extract(h)
      all(t)
    }
    _ -> panic as "`ramble.all()` doesn't accept an empty list of parsers"
  }
}

/// Parse an exact string of characters.
pub fn string(s: String) -> Parser(String, String) {
  // I tried many algorithms for this and this one definitely won,
  // even though it looks bad. 
  // The need to change the line numbers in the position correctly makes 
  // things tricky and slows down the algorithms that seem faster. 
  // Writing this in terms of other parsers handles that automatically.
  case string.pop_grapheme(s) {
    Ok(#(h, t)) -> {
      use c <- extract(token(h))
      use rest <- extract(string(t))
      return(c <> rest)
    }
    Error(_) -> return("")
  }
}

/// Negate a parser: if it succeeds, this fails, and vice versa.
/// This is useful for keyword parsing.
/// Example: `seq(string("if"), not(either(alphanum(), char("_"))))`
pub fn not(p: Parser(t, a)) -> Parser(t, Nil) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p, source, pos, config, recovery)
    case res.value {
      Ok(_) ->
        case source {
          [h, ..] -> {
            let err = Unexpected(pos, Token(h), [])
            ParseResult(
              value: Error(err),
              rest: source,
              new_pos: pos,
              errors: [],
            )
          }
          _ -> {
            let err = Unexpected(pos, Token(config.eof), [])
            ParseResult(
              value: Error(err),
              rest: source,
              new_pos: pos,
              errors: [],
            )
          }
        }
      // todo: better error message here
      Error(_) ->
        ParseResult(value: Ok(Nil), rest: source, new_pos: pos, errors: [])
    }
  })
}

/// Parses successfully only when at the end of the input string.
pub fn end() -> Parser(t, Nil) {
  Parser(fn(source, pos, config, recovery) {
    case source {
      [] -> ParseResult(value: Ok(Nil), rest: [], new_pos: pos, errors: [])
      [h, ..] -> {
        let err = Unexpected(pos, Token(h), [Token(config.eof)])
        // TODO: recovery in this parser doesn't really make sense. 
        // It may be that the ParseResult should have an empty `rest`, 
        // immediately "jumping" to the end of the input.
        let res = run_on_tokens(recover(), source, pos, config, recovery)
        ParseResult(
          value: Error(err),
          rest: res.rest,
          new_pos: res.new_pos,
          errors: [err],
        )
      }
    }
  })
}

/// Run a parser as normal, but the parser itself isn't evaluated until it is used.
/// This is needed for recursive grammars, such as `E := n | E + E` where `n` is a number.
/// Example: `lazy(digit)` instead of `digit()`.
pub fn lazy(p: fn() -> Parser(t, a)) -> Parser(t, a) {
  Parser(fn(source, pos, config, recovery) {
    run_on_tokens(p(), source, pos, config, recovery)
  })
}

/// A monadic bind for pleasant interplay with gleam's `use` syntax.
/// example: 
/// ```
/// fn identifier() -> Parser(String, String) {
///     use pos <- extract(pos())
///     use first <- extract(lowercase_letter())
///     use rest <- extract(many(either(alphanum(), char("_"))))
///     return(Ident(pos, first <> string.concat(rest)))
/// }
/// ```
pub fn extract(p: Parser(t, a), f: fn(a) -> Parser(t, b)) -> Parser(t, b) {
  Parser(fn(source, pos, config, recovery) {
    let recovery_parser = case recovery {
      Some(r) -> r
      None -> any_token()
    }
    run_on_tokens(
      extract_and_recover(p, recovery_parser, f),
      source,
      pos,
      config,
      recovery,
    )
  })
}

pub fn extract_and_recover(
  p: Parser(t, a),
  recovery r: Parser(t, t),
  then f: fn(a) -> Parser(t, c),
) -> Parser(t, c) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p, source, pos, config, Some(r))
    case res.value {
      Ok(v) -> run_on_tokens(f(v), res.rest, res.new_pos, config, recovery)
      Error(e) ->
        ParseResult(
          value: Error(e),
          rest: source,
          new_pos: pos,
          errors: res.errors,
        )
    }
  })
}

/// A monadic return for pleasant interplay with gleam's `use` syntax.
/// see `extract` and `extract_and_recover` for more details and examples.
pub fn return(x: a) -> Parser(t, a) {
  Parser(fn(source, pos, _config, _recovery) {
    ParseResult(value: Ok(x), rest: source, new_pos: pos, errors: [])
  })
}

/// Immediately fail regardless of the next input.
pub fn fail() -> Parser(a, e) {
  Parser(fn(source, pos, config, recovery) {
    let unexpected_token = case source {
      [] -> config.eof
      [h, ..] -> h
    }
    let err = Unexpected(pos, Token(unexpected_token), [])
    let res = run_on_tokens(recover(), source, pos, config, recovery)
    ParseResult(
      value: Error(err),
      rest: res.rest,
      new_pos: res.new_pos,
      errors: [err],
    )
  })
}

/// Parse zero or more repetitions of a parser, collecting the results into a list.
/// Stop when the terminator parser succeeds, even if the looping parser would also succeed.
/// The terminator parser's results are not consumed.
/// The main motivator for `until` is multiline comments ending in `*/`, `-->`, `-}`, `*)`, etc.
pub fn until(
  do p: Parser(t, a),
  until terminator: Parser(t, t),
) -> Parser(t, List(a)) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(terminator, source, pos, config, recovery)
    case res.value {
      Ok(_) ->
        ParseResult(value: Ok([]), rest: source, new_pos: pos, errors: [])
      Error(_) -> {
        let parser = {
          use pos1 <- extract(position())
          use first <- extract_and_recover(p, recovery: terminator)
          use pos2 <- extract(position())
          io.debug(#(1, pos1, pos2))
          use rest <- extract(until(p, terminator))
          return([first, ..rest])
        }
        run_on_tokens(parser, source, pos, config, recovery)
      }
    }
  })
}

pub fn peek(p: Parser(t, a)) -> Parser(t, Bool) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p, source, pos, config, recovery)
    case res.value {
      Ok(_) ->
        ParseResult(value: Ok(True), rest: source, new_pos: pos, errors: [])
      Error(_) ->
        ParseResult(value: Ok(False), rest: source, new_pos: pos, errors: [])
    }
  })
}

pub fn label(label: String, p: fn() -> Parser(t, a)) -> Parser(t, a) {
  Parser(fn(source, pos, config, recovery) {
    let res = run_on_tokens(p(), source, pos, config, recovery)
    case res.value {
      Ok(_) -> res
      Error(e) -> {
        let e = case e {
          Unexpected(pos, t, _) -> Unexpected(pos, t, [LabeledToken(label)])
        }
        ParseResult(
          value: Error(e),
          rest: source,
          new_pos: pos,
          errors: res.errors,
        )
      }
    }
  })
}

/// This grants full control over parsing by allowing a custom parse function.
/// This is like making the Parser type not-opaque.
/// Use this with caution, and be sure to mind the invariants of the library.
/// It should be very rare that you need to use this.
pub fn desperate_custom_parser(
  parse: fn(List(t), Position, ParserConfig(t), Option(Parser(t, t))) ->
    ParseResult(t, a),
) -> Parser(t, a) {
  Parser(parse)
}

fn pretty_expectations(head: String, tail: List(String)) -> String {
  case tail {
    [] -> head
    [x] -> head <> " or " <> x
    [x, y] -> head <> ", " <> x <> ", or " <> y
    [x, ..rest] -> head <> ", " <> pretty_expectations(x, rest)
  }
}

fn pretty_err(
  err: ParseError(t),
  show_token: fn(t) -> String,
  source_name: String,
) -> String {
  let show = fn(token: Token(t)) {
    case token {
      Token(t) -> show_token(t)
      LabeledToken(s) -> s
    }
  }
  case list.map(err.expected, show) {
    [] ->
      "I didn't expect the `"
      <> show(err.token)
      <> "` at position "
      <> int.to_string(err.pos.row)
      <> ":"
      <> int.to_string(err.pos.col)
      <> " in "
      <> source_name
      <> "."
    [hd, ..tl] ->
      "I didn't expect the `"
      <> show(err.token)
      <> "` at position "
      <> int.to_string(err.pos.row)
      <> ":"
      <> int.to_string(err.pos.col)
      <> " in "
      <> source_name
      <> ". I expected "
      <> pretty_expectations(hd, tl)
      <> "."
  }
}

pub fn main() {
  let config =
    ParserConfig(
      newline: "\n",
      eof: "EOF",
      source_name: "DEBUG",
      token_to_string: fn(s) { s },
    )
  let FinalParseResult(v, errs) =
    go(seq(do_and_recover(token("c"), recovery: token(";")), token("a")), "ab;c", config)
  io.debug(#(v, errs))
  let errs = case v {
    Error(e) -> [e, ..errs]
    _ -> errs
  }
  use it <- list.each(errs)
  io.println(pretty_err(it, config.token_to_string, config.source_name))
}
