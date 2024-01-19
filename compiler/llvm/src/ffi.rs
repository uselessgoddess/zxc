use libc::c_uint;

pub type Bool = c_uint;

pub const True: Bool = 1 as Bool;
pub const False: Bool = 0 as Bool;

extern "C" {
    pub fn LLVMIsMultithreaded() -> Bool;
}
