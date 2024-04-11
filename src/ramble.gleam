// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/string
import gleam/list
import gleam/io
import gleam/int
import gleam/option.{type Option, None, Some}

/// The type for positions within a string.
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

/// The parser type, parameterized by the type it parses and 
/// the user-defined error type it can return.
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

pub fn go_on_tokens(
  p: Parser(t, a),
  src: List(t),
  config: ParserConfig(t),
) -> FinalParseResult(t, a) {
  let res = p.parse(src, Position(1, 1), config, None)
  FinalParseResult(value: res.value, errors: res.errors)
}

/// Apply a parser to a string.
pub fn go(
  p: Parser(String, a),
  src: String,
  config: ParserConfig(String),
) -> FinalParseResult(String, a) {
  go_on_tokens(p, string.to_graphemes(src), config)
}

/// Get the current parser position.
pub fn position() -> Parser(t, Position) {
  Parser(fn(source, p, _config, _recovery) {
    ParseResult(value: Ok(p), rest: source, new_pos: p, errors: [])
  })
}

pub fn get_errors(res: ParseResult(t, a)) -> List(ParseError(t)) {
  case res.value {
    Ok(_) -> res.errors
    Error(e) -> [e, ..res.errors]
  }
}

pub fn label(label: String, p: fn() -> Parser(t, a)) -> Parser(t, a) {
  Parser(fn(source, pos, config, recovery) {
    let res = p().parse(source, pos, config, recovery)
    case res.value {
      Ok(_) -> res
      Error(e) -> {
        let e = case e {
          Unexpected(pos, t, _) -> Unexpected(pos, t, [LabeledToken(label)])
        }
        ParseResult(Error(e), res.rest, res.new_pos, res.errors)
      }
    }
  })
}

pub fn consume_and_discard_until(p: Parser(t, t)) -> Parser(t, Nil) {
  Parser(fn(source, pos, config, recovery) {
    let res = p.parse(source, pos, config, recovery)
    case res.value {
      Ok(_) ->
        ParseResult(value: Ok(Nil), rest: source, new_pos: pos, errors: [])
      Error(_) ->
        consume_and_discard_until(p).parse(
          res.rest,
          res.new_pos,
          config,
          recovery,
        )
    }
  })
}

/// Do `p`, then apply `f` to the result if it succeeded.
pub fn map(p: Parser(t, a), f: fn(a) -> b) -> Parser(t, b) {
  Parser(fn(source, pos, config, recovery) {
    let res = p.parse(source, pos, config, recovery)
    case res.value {
      Ok(v) -> ParseResult(Ok(f(v)), res.rest, res.new_pos, res.errors)
      Error(e) -> ParseResult(Error(e), source, pos, res.errors)
    }
  })
}

pub fn any_token() -> Parser(t, t) {
  Parser(fn(source, pos, config, _recovery) {
    let assert Position(row, col) = pos
    case source {
      [head, ..tail] -> {
        let new_pos = case head == config.newline {
          True -> Position(row + 1, 0)
          False -> Position(row, col + 1)
        }
        ParseResult(value: Ok(head), rest: tail, new_pos: new_pos, errors: [])
      }
      [] -> {
        let err = Unexpected(pos, Token(config.eof), [])
        ParseResult(value: Error(err), rest: source, new_pos: pos, errors: [])
      }
    }
  })
}

pub fn recover() -> Parser(t, Nil) {
  Parser(fn(source, pos, config, recovery) {
    let p = case recovery {
      Some(r) -> {
        consume_and_discard_until(r)
      }
      None -> {
        any_token()
        |> map(fn(_) { Nil })
      }
    }
    let res = p.parse(source, pos, config, None)
    ParseResult(Ok(Nil), res.rest, res.new_pos, [])
  })
}

/// Parse a character if it matches the predicate.
pub fn satisfy(when pred: fn(t) -> Bool) -> Parser(t, t) {
  Parser(fn(source, pos, config, recovery) {
    case source {
      [h, ..t] ->
        case pred(h) {
          True -> {
            let Position(row, col) = pos
            let new_pos = case h == config.newline {
              True -> Position(row + 1, 0)
              False -> Position(row, col + 1)
            }
            ParseResult(value: Ok(h), rest: t, new_pos: new_pos, errors: [])
          }
          False -> {
            let err = Unexpected(pos, Token(h), [])
            let res = recover().parse(source, pos, config, recovery)
            ParseResult(Error(err), res.rest, res.new_pos, [])
          }
        }
      [] -> {
        let err = Unexpected(pos, Token(config.eof), [])
        ParseResult(value: Error(err), rest: source, new_pos: pos, errors: [])
      }
    }
  })
}

/// Parse a specific character.
pub fn token(t: t) -> Parser(t, t) {
  use config <- extract(get_config())
  use <- label("a '" <> config.token_to_string(t) <> "'")
  satisfy(when: fn(x) { x == t })
}

pub fn extract(p: Parser(t, a), k: fn(a) -> Parser(t, b)) -> Parser(t, b) {
  Parser(fn(source, pos, config, recovery) {
    let res = p.parse(source, pos, config, recovery)
    case res.value {
      Ok(v) -> k(v).parse(res.rest, res.new_pos, config, recovery)
      Error(e) -> ParseResult(Error(e), res.rest, res.new_pos, res.errors)
    }
  })
}

pub fn return(a: a) -> Parser(t, a) {
  Parser(fn(source, pos, _config, _recovery) {
    ParseResult(Ok(a), source, pos, [])
  })
}

pub fn seq(p1: Parser(t, a), p2: Parser(t, b)) -> Parser(t, b) {
  Parser(fn(source, pos, config, recovery) {
    let res1 = p1.parse(source, pos, config, recovery)
    let res2 = p2.parse(res1.rest, res1.new_pos, config, recovery)
    let errors = list.append(get_errors(res1), res2.errors)
    ParseResult(res2.value, res2.rest, res2.new_pos, errors)
  })
}

pub fn do_and_recover(p: Parser(t, a), recovery: Parser(t, t)) -> Parser(t, a) {
  Parser(fn(source, pos, config, _recovery) {
    p.parse(source, pos, config, Some(recovery))
  })
}

pub fn either(p1: Parser(t, a), p2: Parser(t, a)) -> Parser(t, a) {
  Parser(fn(source, pos, config, recovery) {
    let res1 = p1.parse(source, pos, config, recovery)
    case res1.value {
      Ok(_) -> res1
      Error(e1) -> {
        let res2 = p2.parse(source, pos, config, recovery)
        case res2.value {
          Ok(_) -> res2
          Error(e2) ->
            ParseResult(
              value: Error(Unexpected(
                pos: pos,
                token: e2.token,
                expected: list.append(e1.expected, e2.expected),
              )),
              rest: res2.rest,
              new_pos: res2.new_pos,
              errors: [],
            )
        }
      }
    }
  })
}

pub fn char_in_range(from from: Int, until to: Int) -> Parser(String, String) {
  satisfy(fn(c) {
    let assert [codepoint] = string.to_utf_codepoints(c)
    let n = string.utf_codepoint_to_int(codepoint)
    from < n && n < to
  })
}

/// Parse a lowercase letter.
pub fn lowercase_letter() -> Parser(String, String) {
  use <- label("a lowercase letter")
  char_in_range(0x61, 0x7b)
}

/// Parse an uppercase letter.
pub fn uppercase_letter() -> Parser(String, String) {
  use <- label("an uppercase letter")
  char_in_range(0x41, 0x5b)
}

/// Parse a lowercase or uppercase letter.
pub fn letter() -> Parser(String, String) {
  use <- label("a letter")
  either(lowercase_letter(), uppercase_letter())
}

pub fn an_token(c: String) -> Parser(String, String) {
  use <- label("an '" <> c <> "'")
  token(c)
}

/// Parse a digit.
pub fn digit() -> Parser(String, String) {
  use <- label("a digit")
  char_in_range(0x30, 0x3a)
}

/// Parse with the first parser in the list that doesn't fail.
pub fn choice(ps: List(Parser(t, a))) -> Parser(t, a) {
  Parser(fn(source, pos, config, recovery) {
    let res = choice_helper(ps, []).parse(source, pos, config, recovery)
    case res.value {
      Ok(_) -> res
      Error(e) -> {
        let res2 = recover().parse(source, pos, config, recovery)
        ParseResult(Error(e), res2.rest, res2.new_pos, res.errors)
      }
    }
  })
}

fn choice_helper(
  ps: List(Parser(t, a)),
  expected: List(Token(t)),
) -> Parser(t, a) {
  Parser(fn(source, pos, config, recovery) {
    case ps {
      [] -> panic as "`ramble.choice()` doesn't accept an empty list of parsers"
      [p] -> {
        let res = p.parse(source, pos, config, recovery)
        case res.value {
          Ok(_) -> res
          Error(e) -> {
            let expectations = list.append(expected, e.expected)
            let err = Unexpected(e.pos, e.token, expectations)
            ParseResult(Error(err), source, pos, [])
          }
        }
      }
      [p, ..t] -> {
        let res = p.parse(source, pos, config, recovery)
        case res.value {
          Ok(v) ->
            ParseResult(value: Ok(v), rest: source, new_pos: pos, errors: [])
          Error(e) ->
            choice_helper(t, list.append(e.expected, expected)).parse(
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

/// Parse an exact string of characters.
pub fn exact(s: String) -> Parser(String, String) {
  // I tried many algorithms for this and this one definitely won,
  // even though it looks bad. 
  // The need to change the line numbers in the position correctly makes 
  // things tricky and slows down the algorithms that seem faster. 
  // Writing this in terms of other parsers handles that automatically.
  use <- label("\"" <> s <> "\"")
  case string.pop_grapheme(s) {
    Ok(#(h, t)) -> {
      use c <- extract(token(h))
      use rest <- extract(exact(t))
      return(c <> rest)
    }
    Error(_) -> return("")
  }
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
  let p =
    seq(
      do_and_recover(exact("def"), token(";")),
      seq(
        token(";"),
        seq(
          choice([
            uppercase_letter(),
            an_token("a"),
            either(digit(), an_token("r")),
          ]),
          token("d"),
        ),
      ),
    )
  io.println(
    "
Parser code:
seq(
  do_and_recover(exact(\"def\"), token(\";\")),
  seq(
    token(\";\"),
    seq(
      choice([
        uppercase_letter(),
        an_token(\"a\"),
        either(digit(), an_token(\"r\")),
      ]),
      token(\"d\"),
    ),
  ),
)",
  )
  let code = "ab;cd"
  io.println("Parsing: " <> code)
  let FinalParseResult(v, errs) = go(p, code, config)
  let errs = case v {
    Error(e) -> list.append(errs, [e])
    Ok(v) -> {
      io.println("Parser was able to produce a final result: " <> v)
      errs
    }
  }
  use it <- list.each(errs)
  io.println(pretty_err(it, config.token_to_string, config.source_name))
}
