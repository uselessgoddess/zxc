use {
    crate::error::EarlyErrorHandler,
    clap::{Parser, ValueEnum},
    compiler::sess::OptionDescrs,
    std::{collections::HashSet, path::PathBuf},
};

#[derive(ValueEnum, Copy, Clone, Debug)]
pub enum Emit {
    Mir,
    IR,
}

#[derive(Parser, Debug)]
#[command(about)]
pub struct Args {
    pub input: PathBuf,

    #[arg(short)]
    pub output: Option<PathBuf>,

    #[arg(long = "out-dir")]
    pub output_dir: Option<PathBuf>,

    #[arg(long = "emit")]
    pub emit: Vec<Emit>,

    /// codegen flags to zxc
    #[arg(short = 'C', value_name = "FLAG", value_parser = parse_kv)]
    pub c_flags: Vec<(String, Option<String>)>,

    /// compiler flags to zxc
    #[arg(short = 'Z', value_name = "FLAG", value_parser = parse_kv)]
    pub z_flags: Vec<(String, Option<String>)>,
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
                        None => handler.early_fatal(format!(
                            "{output_name} option `{key}` \
                                requires {type_desc} ({prefix} {key}=<value>)"
                        )),
                        Some(value) => handler.early_fatal(format!(
                            "incorrect value `{value}` for \
                                {output_name} option `{key}` - {type_desc} was expected"
                        )),
                    }
                }
            }
            None => handler.early_fatal(format!("unknown {output_name} option: `{key}`")),
        }
    }
    return op;
}
