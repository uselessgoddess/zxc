use {
    clap::{Parser, ValueEnum},
    middle::sess::{EarlyErrorHandler, OptionDescrs},
    std::path::PathBuf,
};

#[derive(ValueEnum, Copy, Clone, Debug)]
pub enum Emit {
    Mir,
    IR,
}

#[derive(ValueEnum, Copy, Clone, Debug)]
pub enum Color {
    Auto,
    Always,
    Never,
}

#[derive(Parser, Debug)]
#[command(about)]
#[command(styles = super::style::CLAP)]
pub struct Args {
    pub input: PathBuf,

    #[arg(short)]
    pub output: Option<PathBuf>,

    #[arg(long = "out-dir", value_name = "OUTPUT")]
    pub output_dir: Option<PathBuf>,

    #[arg(long = "color", value_name = "WHEN")]
    pub color: Option<Color>,

    /// target triple for which the code is compiled
    #[arg(long = "target")]
    pub target: Option<String>,

    #[arg(long = "emit", value_parser = parse_emit)]
    pub emit: Vec<(Emit, Option<String>)>,

    /// codegen flags to zxc
    #[arg(short = 'C', value_name = "FLAG", value_parser = parse_kv)]
    pub c_flags: Vec<(String, Option<String>)>,

    /// compiler flags to zxc
    #[arg(short = 'Z', value_name = "FLAG", value_parser = parse_kv)]
    pub z_flags: Vec<(String, Option<String>)>,

    /// Set lint allowed
    #[arg(short = 'A', long, value_name = "LINT")]
    pub allow: Vec<String>,

    /// Set lint warnings
    #[arg(short = 'W', long, value_name = "LINT")]
    pub warn: Vec<String>,

    /// Set lint denied
    #[arg(short = 'D', long, value_name = "LINT")]
    pub deny: Vec<String>,
}

fn parse_emit(s: &str) -> Result<(Emit, Option<String>), String> {
    Ok(match s.split_once('=') {
        None => (Emit::from_str(s, true)?, None),
        Some((k, v)) => (Emit::from_str(k, true)?, Some(v.into())),
    })
}

fn parse_kv(s: &str) -> Result<(String, Option<String>), !> {
    Ok(match s.split_once('=') {
        None => (s.into(), None),
        Some((k, v)) => (k.into(), Some(v.into())),
    })
}

pub fn build_options<O: Default>(
    handler: &EarlyErrorHandler,
    matches: &[(String, Option<String>)],
    descrs: OptionDescrs<O>,
    prefix: &str,
    output_name: &str,
) -> O {
    let mut op = O::default();
    for (key, value) in matches {
        let value = value.as_deref();

        let option_to_lookup = key.replace('-', "_");
        match descrs.iter().find(|(name, ..)| *name == option_to_lookup) {
            Some((_, setter, type_desc, _)) => {
                if !setter(&mut op, value) {
                    match value {
                        None => handler.early_error(format!(
                            "{output_name} option `{key}` \
                                requires {type_desc} ({prefix} {key}=<value>)"
                        )),
                        Some(value) => handler.early_error(format!(
                            "incorrect value `{value}` for \
                                {output_name} option `{key}` - {type_desc} was expected"
                        )),
                    }
                }
            }
            None => handler.early_error(format!("unknown {output_name} option: `{key}`")),
        }
    }
    op
}
