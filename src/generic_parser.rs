//! Holds code related to parsers which can be used in any program and not necessarily tied to this.
use std::ops::{Range, RangeFrom, RangeTo};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::{
        complete::{char, line_ending, multispace0, space0},
        is_alphanumeric,
    },
    combinator::opt,
    error::{context, ContextError, ParseError},
    multi::separated_list0,
    sequence::{delimited, preceded, separated_pair, terminated},
    AsChar, Compare, IResult, InputIter, InputLength, InputTakeAtPosition, Parser, Slice,
};

/// Remove optional line ending around a given parser.
pub fn ole<'a, I, O, E, F>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    E: ParseError<I>,
    I: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    I: InputIter + InputLength,
    I: Compare<&'static str>,
    I: Clone,
{
    delimited(opt(line_ending), f, opt(line_ending))
}

/// Remove optional space and tab around a given parser.
pub fn osp<'a, I, O, F, E>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    E: ParseError<I>,
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited(space0, f, space0)
}

/// Remove optional whitespace (spaces or newlines) around a given parser.
pub fn ows<'a, I, O, E, F>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    E: ParseError<I>,
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited(multispace0, f, multispace0)
}

/// Parse a triplet of `first sep second sep third`.
pub fn separated_triplet<I, O1, O2, O3, O4, E, F, G, H, S>(
    mut first: F,
    mut sep: S,
    mut second: G,
    mut third: H,
) -> impl FnMut(I) -> IResult<I, (O1, O3, O4), E>
where
    E: ParseError<I>,
    F: Parser<I, O1, E>,
    S: Parser<I, O2, E>,
    G: Parser<I, O3, E>,
    H: Parser<I, O4, E>,
{
    move |input: I| {
        let (input, o1) = first.parse(input)?;
        let (input, _) = sep.parse(input)?;
        let (input, o2) = second.parse(input)?;
        let (input, _) = sep.parse(input)?;
        third.parse(input).map(|(i, o3)| (i, (o1, o2, o3)))
    }
}

/// See if character is in Greek (lowercase or uppercse) range or not.
pub fn is_greek(input: char) -> bool {
    // from alpha (lowercase and upper) to omega (lower and upper)
    matches!(input, '\u{0391}'..='\u{03A9}' | '\u{03B1}'..='\u{03C9}')
}

/// Is a valid symbol letter as in `[_-A-Za-z1-9Α-Ωα-ω]`
pub fn is_letter(input: char) -> bool {
    is_alphanumeric(input as u8) || input == '_' || input == '-' || is_greek(input)
}

// TODO manage latex variants and make them one using a macro or limited command that only exposes
// tag and input but with less dry code.
/// Match commands with or without a preceding "\\" and terminating with "{}".
pub fn latex_tag<'a, E>(input: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    move |i: &'a str| {
        terminated(
            preceded(context("optional backslash", opt(tag("\\"))), tag(input)),
            opt(context(
                "optional curly braces",
                delimited(tag("{"), space0, tag("}")),
            )),
        )(i)
    }
}

/// Match commands with or without a preceding "\\" and terminating with "{}".
pub fn latex_symbol<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    terminated(
        preceded(context("optional backslash", opt(tag("\\"))), symbol),
        opt(context(
            "optional curly braces",
            delimited(tag("{"), space0, tag("}")),
        )),
    )(input)
}

/// Parse a word (used for variables and symbols).
pub fn symbol<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    context("symbol parsing", take_while1(is_letter))(input)
}

/// A simple arrow parser.
pub fn arrow<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "mandatory arrow",
        alt((tag("->"), latex_tag("implies"), latex_tag("rightarrow"))),
    )(input)
}

// A list of {I, ...,?}.
pub fn curly_list<'a, F, I, E>(values: F) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<I>, E>
where
    F: Parser<&'a str, I, E>,
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    terminated(
        preceded(
            context("opening curly brace", osp(char('{'))),
            context(
                "comma separated values",
                separated_list0(
                    context("comma delimiter between values", ows(char(','))),
                    context("values", ows(values)),
                ),
            ),
        ),
        terminated(
            context("optional ending comma delimiter", opt(ows(char(',')))),
            context("closing curly brace", osp(char('}'))),
        ),
    )
}

/// Parse a `V = x`.
pub fn variable_definition<'a, F, G, O2, E>(
    variable: F,
    value: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, (&'a str, O2), E>
where
    F: Parser<&'a str, &'a str, E>,
    G: Parser<&'a str, O2, E>,
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "variable definition",
        separated_pair(
            osp(variable),
            char('='),
            terminated(
                context("set right hand side definition", osp(value)),
                context("new line character at the end (LF/CRLF)", line_ending),
            ),
        ),
    )
}

/// Parse a `V = { x_1, ... }`.
pub fn set_definition<'a, F, G, O2, E>(
    variable: F,
    value: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, (&'a str, Vec<O2>), E>
where
    F: Parser<&'a str, &'a str, E>,
    G: Parser<&'a str, O2, E>,
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "set definition",
        variable_definition(variable, curly_list(value)),
    )
}

#[cfg(test)]
mod tests {
    use nom::error::VerboseError;

    use super::*;

    #[test]
    fn test_word() {
        assert_eq!(symbol::<VerboseError<&str>>("hello_"), Ok(("", "hello_")));
        assert_eq!(symbol::<VerboseError<&str>>("he-re"), Ok(("-re", "he")));
        assert_eq!(symbol::<VerboseError<&str>>("he,re"), Ok((",re", "he")));
        assert!(matches!(symbol::<VerboseError<&str>>(",re"), Err(_)));
    }

    #[test]
    fn test_latex() {
        let f = |i| latex_tag::<VerboseError<&str>>("test")(i);
        assert_eq!(f("test"), Ok(("", "test")));
        assert_eq!(f("\\test"), Ok(("", "test")));
        assert_eq!(f("\\test{}"), Ok(("", "test")));
        assert_eq!(f("\\test{ }"), Ok(("", "test")));
        assert_eq!(f("\\test { }"), Ok((" { }", "test")));
        assert_eq!(f("\\test"), Ok(("", "test")));
        assert!(matches!(f("Test"), Err(_))); // case sensitive
        assert!(matches!(f("\\ test"), Err(_)));
        assert!(matches!(f("\\ test { }"), Err(_)));
    }

    #[test]
    fn test_arrow() {
        let f = |i| arrow::<VerboseError<&str>>(i);
        assert_eq!(f("->"), Ok(("", "->")));
        assert_eq!(f("\\rightarrow{}"), Ok(("", "rightarrow")));
        assert_eq!(f("\\rightarrow}"), Ok(("}", "rightarrow")));
        assert_eq!(f("\\rightarrow{"), Ok(("{", "rightarrow")));
        assert_eq!(f("\\implies{}"), Ok(("", "implies")));
        assert!(matches!(f(" ->"), Err(_)));
    }

    #[test]
    fn test_osp() {
        let t = |i: &'static str| tag::<&'static str, &'static str, VerboseError<&str>>(i);
        assert_eq!(osp(t("te"))("  te "), Ok(("", "te")));
        assert_eq!(osp(t("te"))("te"), Ok(("", "te")));
        assert!(matches!(osp(t("te"))(" \n te "), Err(_)));
    }

    #[test]
    fn test_ows() {
        let t = |i: &'static str| tag::<&'static str, &'static str, VerboseError<&str>>(i);
        assert_eq!(ows(t("te"))("  te "), Ok(("", "te")));
        assert_eq!(ows(t("te"))("te"), Ok(("", "te")));
        assert_eq!(ows(t("te"))("  \t\r\nte\n"), Ok(("", "te")));
    }

    #[test]
    fn test_symbol() {
        let f = |i| symbol::<VerboseError<&str>>(i);
        assert_eq!(f("te "), Ok((" ", "te")));
        assert_eq!(f("1te\n"), Ok(("\n", "1te")));
        assert_eq!(f("_te y"), Ok((" y", "_te")));
        assert_eq!(f("1 y"), Ok((" y", "1")));
    }

    #[test]
    fn test_set_definition() {
        let f = |i| set_definition::<_, _, _, VerboseError<&str>>(tag("var"), symbol)(i);
        assert_eq!(f(" var = {} \n"), Ok(("", ("var", vec![]))));
        assert_eq!(f(" var = { v ,3  } \n"), Ok(("", ("var", vec!["v", "3"]))));
        assert_eq!(f(" var = { \nv\n } \n"), Ok(("", ("var", vec!["v"]))));
        assert_eq!(f(" var = { \nv\n, } \n"), Ok(("", ("var", vec!["v"]))));
        assert!(matches!(f(" var = { \nv, }"), Err(_))); // no ending lf

        assert_eq!(f(" var = { v, 3  } \n"), Ok(("", ("var", vec!["v", "3"]))));
        assert_eq!(f(" var = { v, 3,  } \n"), Ok(("", ("var", vec!["v", "3"]))));
        assert_eq!(f(" var = { v , } \n"), Ok(("", ("var", vec!["v"]))));
        assert_eq!(f(" var = { v , 3  } \n"), Ok(("", ("var", vec!["v", "3"]))));
        assert_eq!(
            f(" var = { v , 3 ,  } \n"),
            Ok(("", ("var", vec!["v", "3"])))
        );
    }
}
