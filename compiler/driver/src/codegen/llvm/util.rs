use {middle::Session, std::sync::Once};

static INIT: Once = Once::new();

pub fn init(sess: &Session) {
    unsafe {
        if llvm::LLVMIsMultithreaded() != 1 {
            panic!("LLVM compiled without support for threads");
        }
        INIT.call_once(|| configure_llvm(sess))
    }
}

unsafe fn configure_llvm(sess: &Session) {
    llvm::initialize_available_targets();
}
