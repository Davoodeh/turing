//! A CLI for running examples
use nom::error::VerboseError;
use turing::machinary::Machine;

use std::{env::args, fs, io, process::exit};

const EXIT: &str = "exit";
const CONTINUE: &str = "c";
const STEP: &str = "";

fn main() {
    let filename = args().nth(1).expect("Give your machine code");
    let content = fs::read_to_string(filename).unwrap();
    let machine = match Machine::from_parse::<VerboseError<&str>>(&content) {
        Ok(machine) => machine,
        Err(e) => {
            panic!("\n----------\n{}", e.into_string(&content));
        }
    };

    let mut machine = machine.run();

    println!("Machine initial state: {}", machine);

    // Main CLI loop of the program
    println!(
        "Ran the machine. Type \"{}\" to exit (or ^C as always)",
        EXIT
    );
    println!("\t- To finish the machine, type \"{}\"", CONTINUE);
    println!("\t- To step the machine, type \"{}\"", STEP);
    print!(">> ");
    loop {
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to get STDIN input");

        match input.trim() {
            EXIT => break,
            CONTINUE => {
                match machine.finish_max_timeout() {
                    Ok(true) => println!("Machine finished with this state: {}", machine),
                    Ok(false) => panic!("Machine halted with this state: {}", machine),
                    Err(e) => panic!(
                        "Machine ran out of time, latest state and error: {:#?}\n{:#?}",
                        machine, e
                    ),
                };
                exit(0)
            }
            STEP => {
                let tick = machine.tick();
                println!("Machine after the tick: {}", machine);
                if machine.is_finished.is_none() && tick.is_err() {
                    tick.expect("Machine halted!");
                } else if machine.is_finished.is_some() {
                    println!("Machine finished (exiting...)");
                    exit(0);
                }
            }
            _ => println!(
                "Invalid input, try \"{}\", \"{}\" or \"{}\"",
                EXIT, CONTINUE, STEP
            ),
        }
    }

    println!("{:#?}", machine.tick());
}
