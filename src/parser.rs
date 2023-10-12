//! Holds code related to opinionated parsers used for this code.
use nom::{
    branch::{alt, permutation},
    bytes::complete::{tag, tag_no_case},
    character::complete::char,
    character::complete::{line_ending, multispace0},
    combinator::{all_consuming, map, opt},
    error::{context, convert_error, ContextError, ParseError, VerboseError},
    multi::many1,
    sequence::{separated_pair, terminated},
    IResult,
};

use crate::{
    generic_parser::{
        arrow, latex_symbol, latex_tag, osp, ows, separated_triplet, set_definition, symbol,
        variable_definition,
    },
    machinary::{Instruction, InstructionMap, Machine, MachineError, Motion, Variables},
};

impl Motion {
    pub fn parse<'a, E>(input: &'a str) -> IResult<&'a str, Motion, E>
    where
        E: ParseError<&'a str> + ContextError<&'a str>,
    {
        let (input, possible) = osp(latex_symbol)(input)?;

        // FIXME Find a valid way to construct an error of E instead
        let o = Self::from_str(possible).map_err(|_| {
            context("invalid motion in given line", tag("!NEVERMATCH!"))(input)
                .err()
                .unwrap()
        })?;

        Ok((input, o))
    }
}

/// A code entry parser for `q_x, n -> q_y, y, S`.
pub fn instruction<'a, E>(input: &'a str) -> IResult<&'a str, Instruction<&'a str>, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    let (input, lhs) = separated_pair(osp(symbol::<E>), char(','), osp(symbol::<E>))(input)?;
    let (input, _) = osp(arrow)(input)?;
    let (input, rhs) =
        separated_triplet(osp(symbol), ows(char(',')), osp(symbol), Motion::parse)(input)?;
    let (input, _) = line_ending(input)?;
    Ok((input, (lhs, rhs)))
}

/// Multiple code entries.
pub fn instructions<'a, E>(input: &'a str) -> IResult<&'a str, InstructionMap<&'a str>, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    map(many1(instruction), |v| {
        v.into_iter().collect::<InstructionMap<&str>>()
    })(input)
}

#[derive(Debug, Clone)]
pub enum Error<E> {
    Nom(nom::Err<E>),
    Machine(MachineError),
}

impl Error<VerboseError<&str>> {
    pub fn into_string(self, input: &str) -> String {
        match self {
            Self::Nom(nom::Err::Error(e)) | Self::Nom(nom::Err::Failure(e)) => {
                convert_error(input, e)
            }
            Self::Nom(nom::Err::Incomplete(e)) => format!("Incomplete input: {:?}", e),
            Self::Machine(e) => e.to_string(),
        }
    }
}

impl<E> From<nom::Err<E>> for Error<E> {
    fn from(value: nom::Err<E>) -> Self {
        Self::Nom(value)
    }
}

impl<E> From<MachineError> for Error<E> {
    fn from(value: MachineError) -> Self {
        Self::Machine(value)
    }
}

impl<'a> Machine<'a> {
    /// Parse a full file of variable definitions coming before code definition.
    // TODO update the errors
    pub fn from_parse<E>(input: &'a str) -> Result<Machine<'a>, Error<E>>
    where
        E: ParseError<&'a str> + ContextError<&'a str>,
    {
        let (input, vars) = Self::variables_section(input)?;
        let (input, _) = multispace0(input)?;
        let map = Self::instruction_section(input, &vars)?;
        Ok(Machine { vars, map })
    }

    pub fn instruction_section<'b, E>(
        input: &'a str,
        vars: &'b Variables<'a>,
    ) -> Result<InstructionMap<usize>, Error<E>>
    where
        E: ParseError<&'a str> + ContextError<&'a str>,
    {
        Ok(Machine::indicize_symbolic_map(
            context(
                "instruction section",
                all_consuming(terminated(instructions, multispace0)),
            )(input)?
            .1,
            &vars.states,
            &vars.alphabet,
            &vars.motions,
        )?)
    }

    pub fn variables_section<E>(input: &'a str) -> Result<(&'a str, Variables<'a>), Error<E>>
    where
        E: ParseError<&'a str> + ContextError<&'a str>,
    {
        let (
            input,
            (
                (_, mut states),
                (_, alphabet),
                (_, motions),
                (_, initial_state),
                (_, final_state),
                (_, tape),
                (_, delimiter),
                test,
            ),
        ) = context(
            "required variables section",
            permutation((
                context(
                    "states (Q) definition",
                    set_definition(
                        context(
                            "states possible left hand side names",
                            alt((
                                tag_no_case("states"),
                                tag_no_case("q_s"),
                                tag_no_case("qs"),
                                tag_no_case("q"),
                            )),
                        ),
                        symbol,
                    ),
                ),
                context(
                    "alphabet (Sigma) definition",
                    set_definition(
                        alt((
                            tag_no_case("symbols"),
                            latex_tag("sigma"),
                            latex_tag("Sigma"),
                            tag_no_case("alphabet"),
                            tag_no_case("alpha"),
                            tag_no_case("a"),
                        )),
                        symbol,
                    ),
                ),
                context(
                    "motions (S) definition",
                    set_definition(
                        context(
                            "motions possible left hand side names",
                            alt((
                                tag_no_case("motions"),
                                tag_no_case("movement"),
                                tag_no_case("moves"),
                                tag_no_case("s"),
                            )),
                        ),
                        Motion::parse,
                    ),
                ),
                context(
                    "initial state (Qi) definition",
                    variable_definition(
                        context(
                            "initial state possible left hand side names",
                            alt((
                                tag_no_case("state_initial"),
                                tag_no_case("state_i"),
                                tag_no_case("state_0"),
                                tag_no_case("initial_state"),
                                tag_no_case("initial"),
                                tag_no_case("q_i"),
                                tag_no_case("qi"),
                                tag_no_case("s_i"),
                                tag_no_case("si"),
                                tag_no_case("q_i"),
                                tag_no_case("qi"),
                                tag_no_case("q_0"),
                                tag_no_case("q0"),
                            )),
                        ),
                        symbol,
                    ),
                ),
                context(
                    "final state (Qf) definition",
                    variable_definition(
                        context(
                            "final state possible left hand side names",
                            alt((
                                tag_no_case("final_state"),
                                tag_no_case("final"),
                                tag_no_case("state_final"),
                                tag_no_case("state_f"),
                                tag_no_case("q_f"),
                                tag_no_case("qf"),
                                tag_no_case("s_f"),
                                tag_no_case("sf"),
                            )),
                        ),
                        symbol,
                    ),
                ),
                context(
                    "memory (tape) definition",
                    // "memories (tapes) definition",
                    set_definition(
                        context(
                            "memory possible left hand side names",
                            alt((
                                tag_no_case("streams"),
                                tag_no_case("tapes"),
                                tag_no_case("tape"),
                                tag_no_case("memories"),
                                tag_no_case("memory"),
                                tag_no_case("mem"),
                                tag_no_case("m"),
                                tag_no_case("initial_streams"),
                                tag_no_case("initial_tapes"),
                                tag_no_case("initial_tape"),
                                tag_no_case("initial_memories"),
                                tag_no_case("initial_memory"),
                                tag_no_case("initial_mem"),
                                tag_no_case("initial_m"),
                            )),
                        ),
                        symbol,
                    ),
                ),
                context(
                    "delimiter definition",
                    variable_definition(
                        context(
                            "delimiter possible left hand side names",
                            alt((
                                tag_no_case("delimiter"),
                                tag_no_case("blank"),
                                tag_no_case("b"),
                                tag_no_case("d"),
                            )),
                        ),
                        symbol,
                    ),
                ),
                opt(context(
                    "optional test definition",
                    set_definition(
                        context(
                            "delimiter possible left hand side names",
                            alt((tag_no_case("test"), tag_no_case("t"))),
                        ),
                        symbol,
                    ),
                )),
            )),
        )(input)?;

        let (initial_state, final_state) =
            Self::indicize_states((initial_state, final_state), &mut states)?;
        let tape = Self::indicize_tape(&tape, &alphabet)?;
        let delimiter = Self::indicize_delimiter(delimiter, &alphabet)?;
        let test = match test {
            Some((_, v)) => Some(Self::indicize_tape(&v, &alphabet)?),
            None => None,
        };

        Ok((
            input,
            Variables {
                test,
                alphabet,
                final_state,
                initial_state,
                delimiter,
                motions,
                states,
                tape,
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use nom::error::VerboseError;

    use super::*;

    #[test]
    fn test_command() {
        assert_eq!(
            instruction::<VerboseError<&str>>(" q_1 , 2 -> q_f, 4,up rest"),
            Ok(("rest", (("q_1", "2"), ("q_f", "4", Motion::Up))))
        );
    }
}
