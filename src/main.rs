#![deny(warnings)]
use std::{
    fmt::Display,
    io::{Read, Write},
};

use automated_synthesis_of_probabilistic_termination_proofs::{
    parsers::{self, Parser},
    pts::{variable::Variable, PTS},
    ranking_function_generators::{
        farkas_based::FarkasBasedGenerator,
        linear_solvers::{minilp::Minilp, Solver as LinearSolver, SolverError},
        Generator, GeneratorError, RankedPTS,
    },
};
use clap::{Parser as CLI, ValueEnum};
use clio;

/// Cli for utilizing the associated library
#[derive(CLI, Debug)]
#[command(arg_required_else_help(true), author, version, about)]
struct Cli {
    /// Parser used to parse input program
    #[arg(value_enum)]
    parser: ProgramParser,

    /// Algorithm used for proof synthesis
    #[arg(value_enum, short, long)]
    algorithm: Option<Algorithm>,

    /// Solver used by algorithms which utilize linear solvers
    #[arg(value_enum, short('s'), long, default_value_t = Solver::Minilp, requires="algorithm")]
    linear_solver: Solver,

    // Input program file
    #[arg(short, long, value_parser, default_value = "-")]
    input: clio::Input,

    // Output graphviz file
    #[arg(short, long, value_parser, default_value = "-")]
    output: clio::OutputPath,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum ProgramParser {
    DefaultParser,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Algorithm {
    FarkasBased,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Solver {
    Minilp,
}

struct CliError(String);

enum LibraryError<V: Variable + Display> {
    AlgorithmError(GeneratorError),
    SolverError(SolverError<V>),
}

impl std::fmt::Debug for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn ranking_function_proof<G: Generator, S: LinearSolver>(
    pts: PTS,
) -> Result<RankedPTS, LibraryError<G::VAR>> {
    let problem = G::generate_problem::<S>(&pts).map_err(|e| LibraryError::AlgorithmError(e))?;

    let solution = S::solve(problem).map_err(|e| LibraryError::SolverError(e))?;

    G::build_ranking_function(pts, solution).map_err(|e| LibraryError::AlgorithmError(e))
}

fn with_solver<G: Generator>(arg: Solver, pts: PTS) -> Result<RankedPTS, LibraryError<G::VAR>> {
    match arg {
        Solver::Minilp => ranking_function_proof::<G, Minilp>(pts),
    }
}

fn synthetize_proof(
    algorithm_arg: Algorithm,
    solver_arg: Solver,
    pts: PTS,
) -> Result<Vec<u8>, CliError> {
    let ranked_pts: Result<RankedPTS, CliError> = match algorithm_arg {
        Algorithm::FarkasBased => {
            with_solver::<FarkasBasedGenerator>(solver_arg, pts).map_err(|err| match err {
                LibraryError::AlgorithmError(e) => match e {
                    GeneratorError::InvalidSolution => CliError(format!(
                        "chosen algorithm failed to find a ranking function"
                    )),
                    GeneratorError::InvalidInvariant(invariant) => CliError(format!(
                        "invariant must consist of only inequalities:\n{invariant}"
                    )),
                    gen_err => CliError(gen_err.to_string()),
                },
                LibraryError::SolverError(e) => match e {
                    SolverError::Unbounded => CliError(Default::default()),
                    SolverError::Infeasible => CliError(format!(
                        "chosen algorithm failed to find a ranking function"
                    )),
                    // variable error and relation type errors should be handledby the programmer
                    solver_err => panic!("{solver_err}"),
                },
            })
        }
    };

    let mut output = Vec::<u8>::new();

    dot::render(&ranked_pts?, &mut output).map_err(|e| CliError(format!("{e}")))?;
    Ok(output)
}

fn main() -> Result<(), CliError> {
    let mut args = Cli::parse();
    let mut input = String::default();

    args.input
        .read_to_string(&mut input)
        .map_err(|e| CliError(format!("{e}")))?;

    let pts = match args.parser {
        ProgramParser::DefaultParser => parsers::default::DefaultParser::parse(input.as_str()),
    }
    .map_err(|e| CliError(format!("{e}")))?;

    let mut output_pts = Vec::<u8>::new();

    dot::render(&pts, &mut output_pts).map_err(|e| CliError(format!("{e}")))?;

    if args.algorithm.is_none() {
        // just print the graphviz of input file

        let len = output_pts.len().try_into().map(Some).unwrap_or(None);
        args.output
            .maybe_with_len(len)
            .unwrap()
            .write_all(&output_pts)
            .map_err(|e| CliError(format!("{e}")))?;
        Ok(())
    } else {
        let output_proof: Vec<u8> =
            synthetize_proof(args.algorithm.unwrap(), args.linear_solver, pts)?;

        let len: Option<u64> = (vec![
            output_pts.len().try_into().map(Some).unwrap_or(None),
            output_proof.len().try_into().map(Some).unwrap_or(None),
        ] as Vec<Option<u64>>)
            .into_iter()
            .sum();

        let mut output_handle = args
            .output
            .maybe_with_len(len)
            .map_err(|e| CliError(format!("{e}")))?;
        output_handle
            .write_all(&output_pts)
            .map_err(|e| CliError(format!("{e}")))?;
        output_handle
            .write_all(&output_proof)
            .map_err(|e| CliError(format!("{e}")))?;
        Ok(())
    }
}
