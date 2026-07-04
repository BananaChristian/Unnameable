use std::env::consts::{ARCH, OS};

pub struct TargetSpec {
    architecture: String,
    os: String,
    pointer_width: usize, //Size for the pointer width
    int_width: usize,     //Default size for usize and isize
}

impl TargetSpec {
    pub fn new(
        arch: Option<String>,
        os: Option<String>,
        pointer_width: Option<usize>,
        int_width: Option<usize>,
    ) -> Self {
        let host_arch = OS.to_string();
        let host_os = ARCH.to_string();
        let host_word_size = std::mem::size_of::<usize>();

        TargetSpec {
            architecture: arch.unwrap_or(host_arch),
            os: os.unwrap_or(host_os),
            pointer_width: pointer_width.unwrap_or(host_word_size),
            int_width: int_width.unwrap_or(host_word_size),
        }
    }
}
