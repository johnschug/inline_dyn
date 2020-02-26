use core::mem::{self, MaybeUninit};

use static_assertions::{const_assert, const_assert_eq};
use typenum::{UInt, UTerm, Unsigned, B0, B1, U0, U1, U2, U4, U8};

// Static assertions
const_assert_eq!(DefaultSize::USIZE, mem::size_of::<usize>());

const_assert!(mem::align_of::<<U1 as Align>::Output>() >= 1);
const_assert!(mem::align_of::<<U2 as Align>::Output>() >= 2);
const_assert!(mem::align_of::<<U4 as Align>::Output>() >= 4);
const_assert!(mem::align_of::<<U8 as Align>::Output>() >= 8);
// const_assert!(mem::align_of::<<U16 as Alignment>::Output>() >= 16);

#[cfg(feature = "alloc")]
const _: () = {
    const_assert!(mem::size_of::<alloc::boxed::Box<u8>>() <= mem::size_of::<RawStorage>());
    const_assert!(mem::align_of::<alloc::boxed::Box<u8>>() <= mem::align_of::<RawStorage>());
};

#[cfg(target_pointer_width = "16")]
pub type DefaultSize = typenum::U2;
#[cfg(target_pointer_width = "32")]
pub type DefaultSize = typenum::U4;
#[cfg(target_pointer_width = "64")]
pub type DefaultSize = typenum::U8;
#[cfg(target_pointer_width = "128")]
pub type DefaultSize = typenum::U16;

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

pub unsafe trait Size: Unsigned + Sealed {
    #[doc(hidden)]
    type Output: Copy;
}

unsafe impl Size for UTerm {
    type Output = ();
}

unsafe impl<N: Size> Size for UInt<N, B0> {
    type Output = SizeImplEven<N::Output>;
}

unsafe impl<N: Size> Size for UInt<N, B1> {
    type Output = SizeImplOdd<N::Output>;
}

pub trait Align: Size {
    #[doc(hidden)]
    type Output: Copy;
}

impl Align for U0 {
    type Output = ();
}

impl Align for U1 {
    type Output = u8;
}

impl Align for U2 {
    type Output = u16;
}

impl Align for U4 {
    type Output = u32;
}

impl Align for U8 {
    type Output = u64;
}

// impl Alignment for U16 {
//     type Output = u128;
// }

// const-generic version:
// // Placeholder for builtin type.
// #[derive(Copy, Clone)]
// struct MinAlign<const ALIGN: usize>;
//
// #[repr(C)]
// pub union RawStorage<const SIZE: usize, const ALIGN: usize> {
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
        unsafe {
            Self {
                bytes: MaybeUninit::uninit().assume_init(),
            }
        }
    }

    pub fn as_ptr(&self) -> *const MaybeUninit<u8> {
        unsafe { &self.bytes as *const _ as *const _ }
    }

    pub fn as_mut_ptr(&mut self) -> *mut MaybeUninit<u8> {
        unsafe { &mut self.bytes as *mut _ as *mut _ }
    }
}
