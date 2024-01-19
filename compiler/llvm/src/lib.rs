#[allow(non_snake_case, non_upper_case_globals)]
mod ffi;

pub use ffi::*;

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
