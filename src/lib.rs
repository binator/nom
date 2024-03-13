#![doc = include_str!("../readme.md")]
#![cfg_attr(not(test), no_std)]
#![warn(missing_docs)]
#![deny(clippy::default_numeric_fallback)]

use core::{
  fmt::{
    Display,
    Formatter,
  },
  marker::PhantomData,
};

use binator_core::{
  Contexting,
  Parse,
  Parsed,
};
use nom::{
  Err,
  Needed,
};

/// nom error context
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NomAtom<Error> {
  /// When nom parser reported incomplete
  Incomplete {
    /// How many octet nom parser need to complete its work
    needed: Needed,
  },
  /// When nom parser reported error
  Error {
    /// Error produced by Nom parser.
    error: Error,
  },
}

impl<Error: Display> Display for NomAtom<Error> {
  fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Incomplete { needed } => write!(f, "Nom: Incomplete {:?}", needed),
      Self::Error { error } => write!(f, "Nom: Error {}", error),
    }
  }
}

/// Parser for nom trait.
#[derive(Clone)]
pub struct NomParser<Token, Parser, Error> {
  error: PhantomData<Error>,
  parser: Parser,
  token: PhantomData<Token>,
}

/// Nom trait allow to transform any Nom parser into Binator parser.
pub trait Nom<Token, Stream, Parser, Error>: Sized
where
  Parser: nom::Parser<Stream, Token, Error>,
{
  /// nom method transform any Nom parser into Binator parser.
  /// If nom return incomplete it's considered as irrocoverable error
  /// if you want to try to ask the stream to buffer more data you will
  /// need to do it yourself after catching the error.
  fn nom(self) -> NomParser<Token, Self, Error>;
}

impl<Token, Stream, Context, Parser, Error> Parse<Stream, Context>
  for NomParser<Token, Parser, Error>
where
  Parser: nom::Parser<Stream, Token, Error>,
  Context: Contexting<NomAtom<Error>>,
{
  type Token = Token;

  fn parse(&mut self, stream: Stream) -> Parsed<Token, Stream, Context> {
    match self.parser.parse(stream) {
      Ok((stream, token)) => Parsed::Success { token, stream },
      Err(Err::Error(error)) => Parsed::Failure(Context::new(NomAtom::Error { error })),
      Err(Err::Incomplete(needed)) => Parsed::Error(Context::new(NomAtom::Incomplete { needed })),
      Err(Err::Failure(error)) => Parsed::Error(Context::new(NomAtom::Error { error })),
    }
  }
}

impl<Token, Stream, Parser, Error> Nom<Token, Stream, Parser, Error> for Parser
where
  Parser: nom::Parser<Stream, Token, Error>,
{
  fn nom(self) -> NomParser<Token, Self, Error> {
    NomParser {
      error: PhantomData::default(),
      parser: self,
      token: PhantomData::default(),
    }
  }
}

/// Take a nom parser and return a Binator parser
pub fn nom<Token, Stream, Context, Parser, Error>(
  parser: Parser,
) -> impl Parse<Stream, Context, Token = Token>
where
  Parser: nom::Parser<Stream, Token, Error>,
  Context: Contexting<NomAtom<Error>>,
{
  parser.nom()
}

#[cfg(test)]
mod tests {
  use binator_context::{
    First,
    Keep,
  };
  use binator_core::{
    Contexting,
    Parse,
    Parsed,
  };
  use derive_more::{
    Display,
    From,
  };
  use nom::{
    bytes::complete::{
      tag,
      take_while_m_n,
    },
    combinator::map_res,
    error::{
      Error,
      ErrorKind,
    },
    sequence::tuple,
    IResult,
  };

  use crate::{Nom, NomAtom};

  #[derive(Display, Debug, PartialEq, From)]
  enum FromAtom {
    Nom(NomAtom<Error<&'static str>>),
  }

  type Context = Keep<First, FromAtom>;

  #[derive(Debug, PartialEq)]
  pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
  }

  fn from_hex(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
  }

  fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
  }

  fn hex_primary(input: &str) -> IResult<&str, u8> {
    map_res(take_while_m_n(2, 2, is_hex_digit), from_hex)(input)
  }

  fn hex_color(input: &str) -> IResult<&str, Color> {
    let (input, _) = tag("#")(input)?;
    let (input, (red, green, blue)) = tuple((hex_primary, hex_primary, hex_primary))(input)?;

    Ok((input, Color { red, green, blue }))
  }

  #[test]
  fn parse_color() {
    assert_eq!(
      super::nom::<_, _, Context, _, _>(hex_color).parse("#2F14DF"),
      Parsed::Success {
        stream: "",
        token: Color {
          red: 47,
          green: 20,
          blue: 223,
        }
      }
    );

    assert_eq!(
      hex_color.nom().parse("#2F14D"),
      Parsed::Failure(Context::new(NomAtom::Error {
        error: Error {
          input: "D",
          code: ErrorKind::TakeWhileMN,
        }
      }))
    );
  }
}
