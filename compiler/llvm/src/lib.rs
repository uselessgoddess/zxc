#![feature(extern_types)]
#![allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]

mod ffi;

pub use ffi::*;
use {
    libc::{c_char, size_t},
    std::{
        cell::RefCell,
        ffi::CStr,
        fmt,
        hash::{Hash, Hasher},
        ptr, slice,
        string::FromUtf8Error,
    },
};

#[repr(C)]
pub struct RustString {
    pub bytes: RefCell<Vec<u8>>,
}

impl RustString {
    pub fn len(&self) -> usize {
        self.bytes.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.bytes.borrow().is_empty()
    }
}

/// Appending to a Rust string -- used by RawRustStringOstream.
#[no_mangle]
pub unsafe extern "C" fn LLVMRustStringWriteImpl(
    sr: &RustString,
    ptr: *const c_char,
    size: size_t,
) {
    let slice = slice::from_raw_parts(ptr as *const u8, size);

    sr.bytes.borrow_mut().extend_from_slice(slice);
}

pub fn SetFunctionCallConv(fn_: &Value, cc: CallConv) {
    unsafe {
        LLVMSetFunctionCallConv(fn_, cc as _);
    }
}

pub fn SetUnnamedAddress(global: &Value, unnamed: UnnamedAddr) {
    unsafe {
        LLVMSetUnnamedAddress(global, unnamed);
    }
}

pub fn set_visibility(llglobal: &Value, visibility: Visibility) {
    unsafe {
        LLVMRustSetVisibility(llglobal, visibility);
    }
}

pub fn initialize_available_targets() {
    macro_rules! init_target(
        ($cfg:meta, $($method:ident),*) => { {
            #[cfg($cfg)]
            fn init() {
                extern "C" {
                    $(fn $method();)*
                }
                unsafe {
                    $($method();)*
                }
            }
            #[cfg(not($cfg))]
            fn init() { }
            init();
        } }
    );
    init_target!(
        llvm_component = "x86",
        LLVMInitializeX86TargetInfo,
        LLVMInitializeX86Target,
        LLVMInitializeX86TargetMC,
        LLVMInitializeX86AsmPrinter,
        LLVMInitializeX86AsmParser
    );
    init_target!(
        llvm_component = "arm",
        LLVMInitializeARMTargetInfo,
        LLVMInitializeARMTarget,
        LLVMInitializeARMTargetMC,
        LLVMInitializeARMAsmPrinter,
        LLVMInitializeARMAsmParser
    );
    init_target!(
        llvm_component = "aarch64",
        LLVMInitializeAArch64TargetInfo,
        LLVMInitializeAArch64Target,
        LLVMInitializeAArch64TargetMC,
        LLVMInitializeAArch64AsmPrinter,
        LLVMInitializeAArch64AsmParser
    );
    init_target!(
        llvm_component = "amdgpu",
        LLVMInitializeAMDGPUTargetInfo,
        LLVMInitializeAMDGPUTarget,
        LLVMInitializeAMDGPUTargetMC,
        LLVMInitializeAMDGPUAsmPrinter,
        LLVMInitializeAMDGPUAsmParser
    );
    init_target!(
        llvm_component = "avr",
        LLVMInitializeAVRTargetInfo,
        LLVMInitializeAVRTarget,
        LLVMInitializeAVRTargetMC,
        LLVMInitializeAVRAsmPrinter,
        LLVMInitializeAVRAsmParser
    );
    init_target!(
        llvm_component = "m68k",
        LLVMInitializeM68kTargetInfo,
        LLVMInitializeM68kTarget,
        LLVMInitializeM68kTargetMC,
        LLVMInitializeM68kAsmPrinter,
        LLVMInitializeM68kAsmParser
    );
    init_target!(
        llvm_component = "csky",
        LLVMInitializeCSKYTargetInfo,
        LLVMInitializeCSKYTarget,
        LLVMInitializeCSKYTargetMC,
        LLVMInitializeCSKYAsmPrinter,
        LLVMInitializeCSKYAsmParser
    );
    init_target!(
        llvm_component = "loongarch",
        LLVMInitializeLoongArchTargetInfo,
        LLVMInitializeLoongArchTarget,
        LLVMInitializeLoongArchTargetMC,
        LLVMInitializeLoongArchAsmPrinter,
        LLVMInitializeLoongArchAsmParser
    );
    init_target!(
        llvm_component = "mips",
        LLVMInitializeMipsTargetInfo,
        LLVMInitializeMipsTarget,
        LLVMInitializeMipsTargetMC,
        LLVMInitializeMipsAsmPrinter,
        LLVMInitializeMipsAsmParser
    );
    init_target!(
        llvm_component = "powerpc",
        LLVMInitializePowerPCTargetInfo,
        LLVMInitializePowerPCTarget,
        LLVMInitializePowerPCTargetMC,
        LLVMInitializePowerPCAsmPrinter,
        LLVMInitializePowerPCAsmParser
    );
    init_target!(
        llvm_component = "systemz",
        LLVMInitializeSystemZTargetInfo,
        LLVMInitializeSystemZTarget,
        LLVMInitializeSystemZTargetMC,
        LLVMInitializeSystemZAsmPrinter,
        LLVMInitializeSystemZAsmParser
    );
    init_target!(
        llvm_component = "jsbackend",
        LLVMInitializeJSBackendTargetInfo,
        LLVMInitializeJSBackendTarget,
        LLVMInitializeJSBackendTargetMC
    );
    init_target!(
        llvm_component = "msp430",
        LLVMInitializeMSP430TargetInfo,
        LLVMInitializeMSP430Target,
        LLVMInitializeMSP430TargetMC,
        LLVMInitializeMSP430AsmPrinter,
        LLVMInitializeMSP430AsmParser
    );
    init_target!(
        llvm_component = "riscv",
        LLVMInitializeRISCVTargetInfo,
        LLVMInitializeRISCVTarget,
        LLVMInitializeRISCVTargetMC,
        LLVMInitializeRISCVAsmPrinter,
        LLVMInitializeRISCVAsmParser
    );
    init_target!(
        llvm_component = "sparc",
        LLVMInitializeSparcTargetInfo,
        LLVMInitializeSparcTarget,
        LLVMInitializeSparcTargetMC,
        LLVMInitializeSparcAsmPrinter,
        LLVMInitializeSparcAsmParser
    );
    init_target!(
        llvm_component = "nvptx",
        LLVMInitializeNVPTXTargetInfo,
        LLVMInitializeNVPTXTarget,
        LLVMInitializeNVPTXTargetMC,
        LLVMInitializeNVPTXAsmPrinter
    );
    init_target!(
        llvm_component = "hexagon",
        LLVMInitializeHexagonTargetInfo,
        LLVMInitializeHexagonTarget,
        LLVMInitializeHexagonTargetMC,
        LLVMInitializeHexagonAsmPrinter,
        LLVMInitializeHexagonAsmParser
    );
    init_target!(
        llvm_component = "webassembly",
        LLVMInitializeWebAssemblyTargetInfo,
        LLVMInitializeWebAssemblyTarget,
        LLVMInitializeWebAssemblyTargetMC,
        LLVMInitializeWebAssemblyAsmPrinter,
        LLVMInitializeWebAssemblyAsmParser
    );
    init_target!(
        llvm_component = "bpf",
        LLVMInitializeBPFTargetInfo,
        LLVMInitializeBPFTarget,
        LLVMInitializeBPFTargetMC,
        LLVMInitializeBPFAsmPrinter,
        LLVMInitializeBPFAsmParser
    );
}

pub fn build_string(f: impl FnOnce(&RustString)) -> Result<String, FromUtf8Error> {
    let sr = RustString { bytes: RefCell::new(Vec::new()) };
    f(&sr);
    String::from_utf8(sr.bytes.into_inner())
}

impl Type {
    pub fn i8_llcx(llcx: &Context) -> &Type {
        unsafe { LLVMInt8TypeInContext(llcx) }
    }

    /// Creates an integer type with the given number of bits, e.g., i24
    pub fn ix_llcx(llcx: &Context, num_bits: u64) -> &Type {
        unsafe { LLVMIntTypeInContext(llcx, num_bits as libc::c_uint) }
    }

    pub fn ptr_llcx(llcx: &Context) -> &Type {
        unsafe {
            LLVMPointerTypeInContext(llcx, 0 /* zero address space */)
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other)
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (self as *const Self).hash(hasher);
    }
}

impl Value {
    pub fn type_of(&self) -> &Type {
        unsafe { LLVMTypeOf(self) }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            &build_string(|s| unsafe {
                LLVMRustWriteValueToString(self, s);
            })
            .expect("non-UTF8 value description from LLVM"),
        )
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other)
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            &build_string(|s| unsafe {
                LLVMRustWriteTypeToString(self, s);
            })
            .expect("non-UTF8 type description from LLVM"),
        )
    }
}

#[derive(Copy, Clone, PartialEq)]
#[repr(C)]
pub enum CodeGenOptSize {
    CodeGenOptSizeNone = 0,
    CodeGenOptSizeDefault = 1,
    CodeGenOptSizeAggressive = 2,
}

pub use CodeGenOptSize::*;

pub fn last_error() -> Option<String> {
    unsafe {
        let cstr = LLVMRustGetLastError();
        if cstr.is_null() {
            None
        } else {
            let err = CStr::from_ptr(cstr).to_bytes();
            let err = String::from_utf8_lossy(err).to_string();
            libc::free(cstr as *mut _);
            Some(err)
        }
    }
}
