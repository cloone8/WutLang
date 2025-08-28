//! Compiler for WutLang that compiles WutLang source files into WutLang IL

use std::path::PathBuf;

use ariadne::{Label, Report, Source};
use chumsky::Parser;
use clap::ValueEnum;
use clap::builder::Styles;
use clap::builder::styling::{AnsiColor, Effects, Style};
use log::LevelFilter;
use simplelog::Config;

use crate::parser::SourceFile;

mod parser;

// Styling for clap CLI

/// Header styling
const HEADER: Style = AnsiColor::Green
    .on_default()
    .effects(Effects::UNDERLINE.insert(Effects::BOLD));

/// Usage styling
const USAGE: Style = AnsiColor::BrightYellow.on_default().effects(Effects::BOLD);

/// Literal styling
const LITERAL: Style = AnsiColor::Cyan.on_default().effects(Effects::BOLD);

/// Placeholder styling
const PLACEHOLDER: Style = AnsiColor::Cyan.on_default();

/// Error styling
const ERROR: Style = AnsiColor::BrightRed.on_default().effects(Effects::BOLD);

/// Valid styling
const VALID: Style = AnsiColor::BrightGreen.on_default().effects(Effects::BOLD);

/// Invalid styling
const INVALID: Style = AnsiColor::BrightMagenta.on_default().effects(Effects::BOLD);

/// Context styling
const CONTEXT: Style = AnsiColor::BrightCyan.on_default();

/// Context valeu styling
const CONTEXT_VALUE: Style = LITERAL;

/// Clap CLI styling
const CLAP_STYLING: Styles = Styles::styled()
    .header(HEADER)
    .usage(USAGE)
    .literal(LITERAL)
    .placeholder(PLACEHOLDER)
    .error(ERROR)
    .context(CONTEXT)
    .context_value(CONTEXT_VALUE)
    .valid(VALID)
    .invalid(INVALID);

/// Command line arguments
#[derive(Debug, clap::Parser)]
#[command(version, about, author, styles = CLAP_STYLING)]
struct CliArgs {
    /// Input file
    #[arg(value_hint = clap::ValueHint::FilePath)]
    input: PathBuf,

    /// Output file.
    #[arg(value_hint = clap::ValueHint::FilePath, short, long)]
    output: Option<PathBuf>,

    /// Log level to run with
    #[arg(short, long, value_enum, default_value_t)]
    verbosity: LogLevel,
}

/// CLI-compatible log level
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, Default)]
enum LogLevel {
    /// No logging at all
    Off,

    /// Errors only
    Error,

    /// Warnings or higher
    #[cfg_attr(not(debug_assertions), default)]
    Warn,

    /// Info or higher
    #[cfg_attr(debug_assertions, default)]
    Info,

    /// Debug or higher
    Debug,

    /// All logs
    Trace,
}

impl From<LogLevel> for LevelFilter {
    fn from(value: LogLevel) -> Self {
        match value {
            LogLevel::Off => LevelFilter::Off,
            LogLevel::Error => LevelFilter::Error,
            LogLevel::Warn => LevelFilter::Warn,
            LogLevel::Info => LevelFilter::Info,
            LogLevel::Debug => LevelFilter::Debug,
            LogLevel::Trace => LevelFilter::Trace,
        }
    }
}

fn main() {
    let args = <CliArgs as clap::Parser>::parse();

    if let Err(e) = simplelog::TermLogger::init(
        args.verbosity.into(),
        Config::default(),
        simplelog::TerminalMode::Stderr,
        simplelog::ColorChoice::Auto,
    ) {
        eprintln!("WARNING: Failed to initialize logger. No logs will be produced. Error: {e}");
    }

    let output_file = match &args.output {
        Some(of) => of.clone(),
        None => std::env::current_dir()
            .expect("Failed to determine current working dir")
            .join("out.wil"),
    };

    log::info!(
        "{} -> {}",
        args.input.to_string_lossy(),
        output_file.to_string_lossy()
    );

    let file_content = match std::fs::read_to_string(&args.input) {
        Ok(f) => f,
        Err(e) => {
            log::error!("Failed to read input file due to: {e}");
            return;
        }
    };

    let parsed = {
        profiling::scope!("Parse");

        match SourceFile::parser().parse(&file_content).into_result() {
            Ok(parsed) => parsed,
            Err(errs) => {
                for err in errs {
                    Report::build(
                        ariadne::ReportKind::Error,
                        (args.input.to_string_lossy(), err.span().into_range()),
                    )
                    .with_code(1)
                    .with_label(
                        Label::new((args.input.to_string_lossy(), err.span().into_range()))
                            .with_message(err.reason()),
                    )
                    .finish()
                    .print((args.input.to_string_lossy(), Source::from(&file_content)))
                    .unwrap();
                }

                return;
            }
        }
    };

    println!("{}", parsed.dump());
}
