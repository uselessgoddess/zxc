use {
    super::FunctionCx,
    compiler::{
        mir::{Local, Statement, Ty},
        IndexVec,
    },
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum SsaKind {
    NotSsa,
    MaybeSsa,
}

impl SsaKind {
    pub(crate) fn is_ssa<'tcx>(self, fx: &FunctionCx<'_, '_, 'tcx>, ty: Ty<'tcx>) -> bool {
        self == SsaKind::MaybeSsa && (fx.clif_type(ty).is_some())
    }
}

#[allow(clippy::single_match, clippy::match_single_binding)]
// Now by default all values is possible to be ssa
pub(crate) fn analyze(fx: &FunctionCx<'_, '_, '_>) -> IndexVec<Local, SsaKind> {
    let mut flag_map =
        fx.mir.local_decls.iter().map(|_| SsaKind::MaybeSsa).collect::<IndexVec<Local, SsaKind>>();

    for bb in fx.mir.basic_blocks.iter() {
        for stmt in bb.statements.iter() {
            match stmt {
                Statement::Assign(_place, rvalue) => match rvalue {
                    // TODO: address is one possible case when ssa unavailable
                    // Rvalue::Ref(_, _, place) | Rvalue::AddressOf(_, place) => {
                    //     flag_map[place.local] = SsaKind::NotSsa;
                    // }
                    _ => {}
                },
                _ => {}
            }
        }
    }

    flag_map
}
