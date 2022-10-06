use core::mem::{self, MaybeUninit};

pub use elain::{Align, Alignment};

// static assertions
const _: () = {
    assert!(mem::size_of::<RawStorage<0, 1>>() == 0);
    assert!(mem::size_of::<RawStorage<1, 1>>() == 1);
    assert!(mem::size_of::<RawStorage<2, 1>>() == 2);
    assert!(mem::size_of::<RawStorage<3, 1>>() == 3);
    assert!(mem::size_of::<RawStorage<4, 1>>() == 4);

    assert!(mem::align_of::<RawStorage<0, 1>>() >= 1);
    assert!(mem::align_of::<RawStorage<0, 2>>() >= 2);
    assert!(mem::align_of::<RawStorage<0, 4>>() >= 4);
    assert!(mem::align_of::<RawStorage<0, 8>>() >= 8);
    assert!(mem::align_of::<RawStorage<0, 16>>() >= 16);
    assert!(mem::align_of::<RawStorage<0, 32>>() >= 32);
    assert!(mem::align_of::<RawStorage<0, 64>>() >= 64);
};

/// A constant corresponding to the default size and alignment of the
/// internal storage of an [`InlineDyn`](crate::InlineDyn).
///
/// The value of this integer should be equivalent to the size of a pointer (in bytes).
pub const DEFAULT_SIZE: usize = mem::size_of::<*const u8>();

#[repr(C)]
pub union RawStorage<const SIZE: usize = DEFAULT_SIZE, const ALIGN: usize = SIZE>
where
    Align<ALIGN>: Alignment,
{
    data: [MaybeUninit<u8>; SIZE],
    _align: Align<ALIGN>,
}

impl<const SIZE: usize, const ALIGN: usize> RawStorage<SIZE, ALIGN>
where
    Align<ALIGN>: Alignment,
{
    pub const fn new() -> Self {
        // SAFETY: `data` is an array of `MaybeUninit<u8>` which do not require initialization.
        unsafe {
            Self {
                data: MaybeUninit::uninit().assume_init(),
            }
        }
    }

    pub const fn as_ptr(&self) -> *const MaybeUninit<u8> {
        // SAFETY: `data` is the only variant that is used and is always initialized by Self::new.
        unsafe { &self.data as *const _ as *const _ }
    }

    pub fn as_mut_ptr(&mut self) -> *mut MaybeUninit<u8> {
        // SAFETY: `data` is the only variant that is used and is always initialized by Self::new.
        unsafe { &mut self.data as *mut _ as *mut _ }
    }
}

impl<const SIZE: usize, const ALIGN: usize> Default for RawStorage<SIZE, ALIGN>
where
    Align<ALIGN>: Alignment,
{
    fn default() -> Self {
        Self::new()
    }
}
