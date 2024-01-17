use {
    crate::{
        hir::HirCtx,
        mir::{self, write_mir_pretty},
        pretty::FmtPrinter,
        sess::{OutFileName, OutputType},
    },
    std::{
        fs::File,
        io::{self, Write},
    },
};

pub fn emit_mir<'tcx>(
    hix: &HirCtx<'tcx>,
    mir: &[(mir::DefId, &mir::Body<'tcx>)],
) -> io::Result<()> {
    let mut printer = FmtPrinter::new(hix);

    for &(def, body) in mir {
        write_mir_pretty(def, body, &mut printer).expect("fmt error");
    }

    match hix.tcx.output_filenames().path(OutputType::Mir) {
        OutFileName::Stdout => io::stdout().write_all(printer.into_buf().as_bytes()),
        OutFileName::Real(path) => {
            let mut buf = io::BufWriter::new(File::create(path)?);
            buf.write_all(printer.into_buf().as_bytes())
        }
    }
}
