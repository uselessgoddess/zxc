use {
    cranelift::{
        codegen::{ir, write},
        prelude::isa,
    },
    middle::{errors::EarlyErrorHandler, sess::OutputFilenames},
    std::{
        fs,
        io::{self, Write},
    },
};

pub fn write_ir_file(
    output_filenames: &OutputFilenames,
    name: &str,
    write: impl FnOnce(&mut dyn Write) -> io::Result<()>,
) {
    let outdir = output_filenames.with_extension("clif");
    let res: io::Result<_> = try {
        match fs::create_dir(&outdir) {
            Ok(_) => {}
            Err(err) if err.kind() == io::ErrorKind::AlreadyExists => {}
            res => res?,
        }
        fs::File::create(outdir.join(name)).and_then(|mut file| write(&mut file))?;
    };
    if let Err(err) = res {
        EarlyErrorHandler::new().early_warn(format!("error writing ir file: {}", err));
    }
}

pub fn write_clif_file(
    output_filenames: &OutputFilenames,
    symbol: &str,
    postfix: Option<&str>,
    isa: &dyn isa::TargetIsa,
    func: &ir::Function,
) {
    let file =
        if let Some(p) = postfix { format!("{symbol}.{p}.clif") } else { format!("{symbol}.clif") };
    write_ir_file(output_filenames, &file, |file| {
        let mut clif = String::new();
        write::write_function(&mut clif, func).unwrap();

        for flag in isa.flags().iter() {
            writeln!(file, "set {}", flag)?;
        }
        write!(file, "target {}", isa.triple().architecture)?;
        for isa_flag in isa.isa_flags().iter() {
            write!(file, " {}", isa_flag)?;
        }
        writeln!(file, "\n")?;
        writeln!(file)?;
        file.write_all(clif.as_bytes())?;
        Ok(())
    });
}
