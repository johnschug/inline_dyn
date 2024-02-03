use core::{
    cell::UnsafeCell,
    marker::PhantomPinned,
    mem::{self, ManuallyDrop, MaybeUninit},
    ptr,
};

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
pub(crate) union RawStorage<const SIZE: usize = DEFAULT_SIZE, const ALIGN: usize = SIZE>
where
    Align<ALIGN>: Alignment,
{
    // The `UnsafeCell` here is needed to ensure soundness when storing values that allow
    // interior mutability.
    data: ManuallyDrop<UnsafeCell<MaybeUninit<[u8; SIZE]>>>,
    _align: Align<ALIGN>,
    // This makes `RawStorage: !Unpin` and is needed to ensure soundness when storing values
    // that are `!Unpin`.
    _pin: PhantomPinned,
}

unsafe impl<const SIZE: usize, const ALIGN: usize> Sync for RawStorage<SIZE, ALIGN> where
    Align<ALIGN>: Alignment
{
}

impl<const SIZE: usize, const ALIGN: usize> RawStorage<SIZE, ALIGN>
where
    Align<ALIGN>: Alignment,
{
    pub(crate) fn is_layout_compatible<T: ?Sized>(val: &T) -> bool {
        (mem::size_of_val(val) <= mem::size_of::<Self>())
            && (mem::align_of_val(val) <= mem::align_of::<Self>())
    }

    pub(crate) const fn new() -> Self {
        Self {
            data: ManuallyDrop::new(UnsafeCell::new(MaybeUninit::uninit())),
        }
    }

    pub(crate) const fn as_ptr(&self) -> *const MaybeUninit<u8> {
        // SAFETY: `data` is the only variant that is used and is always initialized by `Self::new()`.
        unsafe { ptr::addr_of!(self.data).cast() }
    }

    pub(crate) fn as_mut_ptr(&mut self) -> *mut MaybeUninit<u8> {
        // SAFETY: `data` is the only variant that is used and is always initialized by `Self::new()`.
        unsafe { ptr::addr_of_mut!(self.data).cast() }
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
