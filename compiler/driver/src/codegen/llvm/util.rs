use {
    middle::Session,
    std::{slice, str, sync::Once},
};

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

fn handle_native(name: &str) -> &str {
    if name != "native" {
        return name;
    }

    unsafe {
        let mut len = 0;
        let ptr = llvm::LLVMRustGetHostCPUName(&mut len);
        str::from_utf8(slice::from_raw_parts(ptr as *const u8, len)).unwrap()
    }
}

pub fn target_cpu(sess: &Session) -> &str {
    // has no overriding now
    handle_native(sess.target.cpu.as_ref())
}
