//! Holds code regarding the logic and workflow of a Turing machine.
use std::{collections::HashMap, fmt};

use crate::traits::Unique;

pub type InstructionInput<T> = (T, T);
pub type InstructionOutput<T> = (T, T, Motion);
/// A transition line, command or instruction from `(a, b) -> (a, b, c)`.
pub type Instruction<T> = (InstructionInput<T>, InstructionOutput<T>);
/// An instruction based on the indices of its collections.
///
/// See [`Machine::indicize_symbolic_map`].
pub type InstructionMap<T> = HashMap<InstructionInput<T>, InstructionOutput<T>>;

pub const DEFAULT_MAX_CLOCK: u128 = 10000;

/// Determines the possible motions of the cursor.
#[derive(Clone, Debug, PartialEq, Display, Copy)]
pub enum Motion {
    /// Move to the upper tape.
    Up,
    /// Move to the lower tape.
    Down,
    Left,
    Right,
    /// Hold still and do not move $\Lambda$.
    None,
}

impl Motion {
    pub fn from_str(input: &str) -> Result<Self, ()> {
        Ok(match input.to_lowercase().as_str() {
            "down" | "d" => Motion::Down,
            "up" | "u" => Motion::Up,
            "left" | "l" => Motion::Left,
            "right" | "r" => Motion::Right,
            "still" | "s" | "\\lambda" | "lambda" | "\u{03BB}" /*λ*/ | "none" | "n" => Motion::None,
            _ => return Err(()),
        })
    }
}

#[derive(Debug)]
pub struct Variables<'a> {
    /// What are the possible states of this machine or $Q$ (excluding halt/crash/falsum/$\bot$).
    pub states: Vec<&'a str>,
    /// What state is the starting state of the machine ($Q_i$).
    pub initial_state: usize,
    /// What state is the ending state of the machine ($Q_f$).
    pub final_state: usize,
    /// What are the acceptable symbols of this machine or $\Sigma$ (must include delimiter).
    pub alphabet: Vec<&'a str>,
    /// Delimiter is the default filler for newly created memory units.
    pub delimiter: usize,
    pub motions: Vec<Motion>,
    // tape_infty: (bool, bool, bool, bool),  // tape infinity from up, right, down, left
    /// Initial inputs of the memory.
    ///
    /// As this is just a part of "variables", one better not edit this. This is not intended for
    /// modification and means to be cloned as a "validated starting point".
    pub tape: Vec<usize>, // tapes: Vec<Vec<usize>>
    pub test: Option<Vec<usize>>, // tests: Vec<Vec<usize>>
}

// TODO
// impl Variables<'_> {
//     pub fn validate_states(&self) {
//         let len = self.initial_state.len();
//         if self.initial_state > len || self.initial_state < len {
//         }
//     }
// }

#[derive(Debug, Display, Clone)]
pub enum Symbol {
    State,
    Alphabet,
    Motion,
}

#[derive(Debug, Display, Clone)]
pub enum MachineError {
    #[display("Unknown {} `{}` in line `{:?} -> {:?}`", _0.to_string().to_ascii_lowercase(), _1, _2, _3)]
    UnknownMapSymbol(
        Symbol,
        String,
        InstructionInput<String>,
        InstructionOutput<String>,
    ),
    #[display("Unknown Symbol `{}` is set for the initial state.", _0)]
    UnknownInitialState(String),
    #[display("Unknown Symbol `{}` is set for the final state.", _0)]
    UnknownFinalState(String),
    #[display("Unknown Symbol `{}` in tape position [{}]", _1, _0)]
    UnknownTapeSymbol(usize, String),
    #[display("Symbol {} is not a valid delimiter defined in the alphabet", _0)]
    UnknownDelimiterSymbol(String),
    #[display("There is no output defined for `{:?}`", _0)]
    NoOutputPartialFunction(InstructionInput<String>),
    #[display("Machine is already finished so it cannot tick any more.")]
    AlreadyFinished,
    #[display(
        "Motion `{}` is not defined in motions of this machine therefore not allowed.",
        _0
    )]
    UnknownMotion(Motion),
}

impl std::error::Error for MachineError {}

#[derive(Debug)]
pub struct Machine<'a> {
    /// Constant definitions of the machine.
    pub vars: Variables<'a>,
    /// Simply leave some part of the map as "not defined" for a crash to happen.
    pub map: InstructionMap<usize>,
}

impl Machine<'_> {
    /// Use indices of collections instead of symbols in the map.
    // TODO optimize
    pub fn indicize_symbolic_map<'a, 'b>(
        symbolic_map: InstructionMap<&'a str>,
        states: &'b Vec<&'a str>,
        alphabet: &'b Vec<&'a str>,
        motions: &'b Vec<Motion>,
    ) -> Result<InstructionMap<usize>, MachineError> {
        macro_rules! search {
            ($needle:ident in $haystack:ident as $type:ident for &$k:ident, &$v:ident) => {
                match $haystack.iter().position(|i| $needle == *i) {
                    Some(value) => Ok(value),
                    None => Err(MachineError::UnknownMapSymbol(
                        Symbol::$type,
                        $needle.to_string(),
                        ($k.0.to_owned(), $k.1.to_owned()),
                        ($v.0.to_owned(), $v.1.to_owned(), $v.2.to_owned()),
                    )),
                }
            };
        }

        let mut indices_map = HashMap::new();

        for (k, v) in symbolic_map {
            let (input_state, input_alpha) = k;
            let (output_state, output_alpha, motion) = v;

            let input_state_index = search!(input_state in states as State for &k, &v)?;
            let input_alpha_index = search!(input_alpha in alphabet as Alphabet for &k, &v)?;
            let output_state_index = search!(output_state in states as State for &k, &v)?;
            let output_alpha_index = search!(output_alpha in alphabet as Alphabet for &k, &v)?;
            let _ = search!(motion in motions as Motion for &k, &v)?;

            indices_map.insert(
                (input_state_index, input_alpha_index),
                (output_state_index, output_alpha_index, motion),
            );
        }

        Ok(indices_map)
    }

    /// Use indices of collections instead of symbols for initial and final states.
    ///
    /// Look for duplicate states or initial_state and final_state not being in the states.
    ///
    /// Returns the indices of initial and final state in the list.
    pub fn indicize_states<'a>(
        (initial_state, final_state): (&'a str, &'a str),
        states: &mut Vec<&'a str>,
    ) -> Result<(usize, usize), MachineError> {
        states.push(initial_state);
        states.push(final_state);
        states.unique();

        let Some(i) = states.iter().position(|&i| i == initial_state) else {
            return Err(MachineError::UnknownInitialState(initial_state.to_owned()));
        };
        let Some(f) = states.iter().position(|&i| i == final_state) else {
            return Err(MachineError::UnknownFinalState(final_state.to_owned()));
        };

        Ok((i, f))
    }

    /// Conver symbols of the tape to indices of alphabet.
    pub fn indicize_tape<'a>(
        tape: &'a Vec<&'a str>,
        alphabet: &'a Vec<&'a str>,
    ) -> Result<Vec<usize>, MachineError> {
        let mut new_tape = vec![];
        for (tape_index, tape_value) in tape.iter().enumerate() {
            let Some(alpha_index) = alphabet.iter().position(|&i| *tape_value == i) else {
                return Err(MachineError::UnknownTapeSymbol(
                    tape_index,
                    tape_value.to_string(),
                ));
            };
            new_tape.push(alpha_index);
        }
        Ok(new_tape)
    }

    /// Convert given delimiter symbol to its index in the alphabet.
    pub fn indicize_delimiter<'a>(
        delimiter: &'a str,
        alphabet: &Vec<&'a str>,
    ) -> Result<usize, MachineError> {
        match alphabet.iter().position(|&i| delimiter == i) {
            Some(v) => Ok(v),
            None => Err(MachineError::UnknownDelimiterSymbol(delimiter.to_owned())),
        }
    }

    pub fn symbolize_tapelike<'b>(
        original: &'b Vec<&'b str>,
        indicized: &'b Vec<usize>,
    ) -> Vec<&'b str> {
        indicized.iter().map(|&i| original[i]).collect()
    }

    pub fn symbolize_input<'b>(
        (state, value): (usize, usize),
        states: &'b Vec<&'b str>,
        alphabet: &'b Vec<&'b str>,
    ) -> InstructionInput<&'b str> {
        (states[state], alphabet[value])
    }
}

// TODO
// impl Machine<'_> {
//     pub fn validate(&self) -> Result<(), MachineError> {
//         // Variables::validate_states();
//     }
// }
//

#[derive(Debug)]
pub struct MachineRun<'a> {
    /// How many single clock operations did the machine do.
    pub clock: u128,
    /// What is the current state of the machine as of now in this clock.
    pub current_state: usize,
    /// Where in the tapes/memory is the machine cursor.
    pub cursor: usize, // cursor: (usize, usize),
    /// Determines whether the machine is done (whether by `Some(false)` as halt or else).
    pub is_finished: Option<bool>,
    /// What are the means for the machine to read or write outputs (memory).
    pub memory: Vec<usize>,
    pub machine: Machine<'a>,
}

impl<'a> MachineRun<'a> {
    /// Create a new machine with a single tape starting from memory 0.
    pub fn new(machine: Machine<'a>) -> Self {
        Self {
            current_state: machine.vars.initial_state,
            cursor: 0,
            clock: 0,
            is_finished: None,
            memory: machine.vars.tape.clone(), // see tape definition for why.
            machine,
        }
    }

    /// Get the input of the cursor position on the current tape.
    ///
    /// As cursor is not meant to move manually, ideally this must never fail. Regardless, it will
    /// panic if cursor is set to an out of bound position in the [`Self::memory`].
    pub fn read_input(&self) -> usize {
        self.memory[self.cursor]
    }

    /// Set the symbol of the cursor position on the current tape.
    ///
    /// As cursor is not meant to move manually, ideally this must never fail. Regardless, it will
    /// panic if cursor is set to an out of bound position in the [`Self::memory`].
    pub fn write_input(&mut self, value: usize) {
        self.memory[self.cursor] = value;
    }

    /// [`Self::read_input`] as its human readable symbol.
    ///
    /// As cursor is not meant to move manually, ideally this must never fail. Regardless, it will
    /// panic if cursor is set to an out of bound position in the [`Self::memory`].
    pub fn symbolize_read_input(&self) -> &str {
        self.machine.vars.alphabet[self.read_input()]
    }

    /// Return the state.
    ///
    /// As cursor is not meant to move manually, ideally this must never fail. Regardless, it will
    /// panic if cursor is set to an out of bound position in the [`Self::memory`].
    pub fn symbolize_current_state(&self) -> &str {
        self.machine.vars.states[self.current_state]
    }

    /// Get the next action based on the current input and state.
    ///
    /// Err means that the function is partial and the machine must halt at this point.
    pub fn action(&self) -> Result<&(usize, usize, Motion), MachineError> {
        let input = (self.current_state, self.read_input());
        match self.machine.map.get(&input) {
            Some(action) => Ok(action),
            None => {
                println!("input: {:?}", input);
                let (q, o) = Machine::symbolize_input(
                    input,
                    &self.machine.vars.states,
                    &self.machine.vars.alphabet,
                );
                Err(MachineError::NoOutputPartialFunction((
                    q.to_owned(),
                    o.to_owned(),
                )))
            }
        }
    }

    /// Convert memory to its symbolized equivalent.
    pub fn symbolize_memory(&self) -> Vec<&str> {
        Machine::symbolize_tapelike(&self.machine.vars.alphabet, &self.memory)
    }

    /// Convert test memory to its symbolized equivalent.
    pub fn symbolize_test(&self) -> Option<Vec<&str>> {
        match self.machine.vars.test {
            Some(ref test) => Some(Machine::symbolize_tapelike(
                &self.machine.vars.alphabet,
                test,
            )),
            None => None,
        }
    }

    /// NOTE this does not check if the motion is valid since it assumes that invalid motions are
    /// filtered out. For example, if S = LR and not Up, it must be checked before reaching this
    /// function.
    pub fn move_cursor(&mut self, motion: Motion) -> Result<(), MachineError> {
        if !self.machine.vars.motions.contains(&motion) {
            return Err(MachineError::UnknownMotion(motion));
        }

        match motion {
            Motion::Up | Motion::Down => return Err(MachineError::UnknownMotion(motion)), // todo
            Motion::Right => {
                self.cursor += 1;
            }
            Motion::Left => {
                self.cursor -= 1;
            }
            Motion::None => {}
        }

        // extend memory if memory is infinite and cursor pushes it.
        // else throw MachineErr::(memory out of bound)
        if self.cursor >= self.memory.len() {
            // todo if infinite
            self.memory.append(&mut vec![
                self.machine.vars.delimiter;
                self.cursor - self.memory.len() + 1
            ]);
            // todo else err
        }

        Ok(())
    }

    /// Make one new process.
    pub fn tick(&mut self) -> Result<(), MachineError> {
        if self.is_finished.is_some() {
            return Err(MachineError::AlreadyFinished);
        }

        let action = match self.action() {
            Ok(action) => action,
            Err(e) => {
                self.is_finished = Some(false);
                return Err(e);
            }
        };

        let (state, value, motion) = action.clone();

        self.write_input(value);
        self.current_state = state;
        self.clock += 1;

        if self.current_state == self.machine.vars.final_state {
            self.is_finished = Some(true);
            return Ok(());
        } // This must come after the if
        self.move_cursor(motion)?; // Ignore movement if finished to prevent out of bound.

        Ok(())
    }

    /// Tick till the clock runs out.
    ///
    /// Returns the machine results as in [`Self::is_finished`] or `Err` if the time taken by the
    /// machine is more than `timeout`.
    pub fn finish(&mut self, timeout: u128) -> Result<bool, ()> {
        loop {
            if self.clock < timeout {
                let _ = self.tick();
            } else {
                return Err(());
            }

            if let Some(is_finished) = self.is_finished {
                return Ok(is_finished);
            }
        }
    }

    /// [`Self::finish`] with [`DEFAULT_MAX_CLOCK`] as the `timeout`.
    pub fn finish_max_timeout(&mut self) -> Result<bool, ()> {
        self.finish(DEFAULT_MAX_CLOCK)
    }

    /// Does the memory matches what is defined in test.
    ///
    /// Returns None if machine is still running or there is no test.
    pub fn is_test_passing(&self) -> Option<bool> {
        if let Some(true) = self.is_finished {
            return None;
        }

        match self.machine.vars.test {
            Some(ref test) => Some(&self.memory == test),
            None => None,
        }
    }
}

impl fmt::Display for MachineRun<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Machine {{\n\
             \tclock: {},\n\
             \tstate: {} ({}),\n\
             \tcursor: [{}] ({:?}),\n\
             \tmemory: {:?},\n\
             {}\
             {}\
             }}",
            self.clock,
            self.symbolize_current_state(),
            match self.is_finished {
                Some(true) => "finished",
                Some(false) => "halted",
                None => "running",
            },
            self.cursor,
            self.symbolize_read_input(),
            self.symbolize_memory(),
            self.symbolize_test()
                .map(|v| format!("\ttest: {:?},\n", v))
                .unwrap_or("".to_owned()),
            match self.machine.vars.test {
                Some(ref test) => format!("\tmatches test: {}\n", &self.memory == test),
                None => "".to_owned(),
            },
        )
    }
}

impl<'a> Machine<'a> {
    /// Start this machine by wrapping it in a [`MachineRun`].
    pub fn run(self) -> MachineRun<'a> {
        MachineRun::new(self)
    }
}
