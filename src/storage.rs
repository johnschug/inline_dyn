use core::mem::{self, MaybeUninit};

use cfg_if::cfg_if;
use static_assertions::{const_assert, const_assert_eq};
use typenum::{UInt, UTerm, Unsigned, B0, B1, U0, U1, U16, U2, U32, U4, U64, U8};

// Static assertions
const_assert_eq!(DefaultSize::USIZE, mem::size_of::<usize>());

const_assert!(mem::align_of::<RawStorage<U0, U1>>() >= 1);
const_assert!(mem::align_of::<RawStorage<U0, U2>>() >= 2);
const_assert!(mem::align_of::<RawStorage<U0, U4>>() >= 4);
const_assert!(mem::align_of::<RawStorage<U0, U8>>() >= 8);
const_assert!(mem::align_of::<RawStorage<U0, U16>>() >= 16);
const_assert!(mem::align_of::<RawStorage<U0, U32>>() >= 32);
const_assert!(mem::align_of::<RawStorage<U0, U64>>() >= 64);

#[cfg(feature = "alloc")]
const _: () = {
    const_assert!(mem::size_of::<alloc::boxed::Box<u8>>() <= mem::size_of::<RawStorage>());
    const_assert!(mem::align_of::<alloc::boxed::Box<u8>>() <= mem::align_of::<RawStorage>());
};

cfg_if! {
    if #[cfg(target_pointer_width = "16")] {
        pub type DefaultSize = typenum::U2;
    } else if #[cfg(target_pointer_width = "32")] {
        pub type DefaultSize = typenum::U4;
    } else if #[cfg(target_pointer_width = "64")] {
        pub type DefaultSize = typenum::U8;
    } else if #[cfg(target_pointer_width = "128")] {
        pub type DefaultSize = typenum::U16;
    } else if #[cfg(target_pointer_width = "256")] {
        pub type DefaultSize = typenum::U32;
    } else if #[cfg(target_pointer_width = "512")] {
        pub type DefaultSize = typenum::U64;
    } else {
        compile_error!("unexpected target_pointer_width");
    }
}

pub trait Sealed {}

impl Sealed for UTerm {}
impl<N: Sealed> Sealed for UInt<N, B1> {}
impl<N: Sealed> Sealed for UInt<N, B0> {}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SizeImplEven<U>(U, U);

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SizeImplOdd<U>(U, U, MaybeUninit<u8>);

/// A marker trait for a [type-level unsigned integer](typenum::uint::UInt) representing a size.
pub unsafe trait Size: Unsigned + Sealed {
    #[doc(hidden)]
    type Output: Copy + Unpin + Send + Sync;
}

unsafe impl Size for UTerm {
    #[doc(hidden)]
    type Output = ();
}

unsafe impl<N: Size> Size for UInt<N, B0> {
    #[doc(hidden)]
    type Output = SizeImplEven<N::Output>;
}

unsafe impl<N: Size> Size for UInt<N, B1> {
    #[doc(hidden)]
    type Output = SizeImplOdd<N::Output>;
}

/// A marker trait for a [type-level unsigned integer](typenum::uint::UInt) representing an
/// alignment.
pub trait Align: Size {
    #[doc(hidden)]
    type Output: Copy + Unpin + Send + Sync;
}

impl Align for U0 {
    #[doc(hidden)]
    type Output = ();
}

impl Align for U1 {
    #[doc(hidden)]
    type Output = u8;
}

#[repr(align(2))]
#[derive(Copy, Clone)]
pub struct Aligned2(u8);

impl Align for U2 {
    #[doc(hidden)]
    type Output = Aligned2;
}

#[repr(align(4))]
#[derive(Copy, Clone)]
pub struct Aligned4(u8);

impl Align for U4 {
    #[doc(hidden)]
    type Output = Aligned4;
}

#[repr(align(8))]
#[derive(Copy, Clone)]
pub struct Aligned8(u8);

impl Align for U8 {
    #[doc(hidden)]
    type Output = Aligned8;
}

#[repr(align(16))]
#[derive(Copy, Clone)]
pub struct Aligned16(u8);

impl Align for U16 {
    #[doc(hidden)]
    type Output = Aligned16;
}

#[repr(align(32))]
#[derive(Copy, Clone)]
pub struct Aligned32(u8);

impl Align for U32 {
    #[doc(hidden)]
    type Output = Aligned32;
}

#[repr(align(64))]
#[derive(Copy, Clone)]
pub struct Aligned64(u8);

impl Align for U64 {
    #[doc(hidden)]
    type Output = Aligned64;
}

// const-generic version:
// // Placeholder for builtin type.
// #[derive(Copy, Clone)]
// struct MinAlign<const ALIGN: usize>;
//
// #[repr(C)]
// pub(crate) union RawStorage<const SIZE: usize, const ALIGN: usize> {
//     bytes: [MaybeUninit<u8>; SIZE],
//     _align: MinAlign<ALIGN>,
// }

#[repr(C)]
pub(crate) union RawStorage<S: Size = DefaultSize, A: Align = S> {
    bytes: S::Output,
    _align: <A as Align>::Output,
}

impl<S: Size, A: Align> RawStorage<S, A> {
    pub fn new() -> Self {
        // SAFETY: `bytes` is an array of `MaybeUninit<u8>` which do not require initialization.
        unsafe {
            Self {
                bytes: MaybeUninit::uninit().assume_init(),
            }
        }
    }

    pub fn as_ptr(&self) -> *const MaybeUninit<u8> {
        // SAFETY: `bytes` is the only variant that is used and is always initialized by new.
        unsafe { &self.bytes as *const _ as *const _ }
    }

    pub fn as_mut_ptr(&mut self) -> *mut MaybeUninit<u8> {
        // SAFETY: `bytes` is the only variant that is used and is always initialized by new.
        unsafe { &mut self.bytes as *mut _ as *mut _ }
    }
}
