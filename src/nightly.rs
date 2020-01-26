#![allow(dead_code)]
use core::marker::Unsize;

use crate::{InlineDyn, VTable};

pub struct Bool<const B: bool>;

pub trait Assert {}

impl Assert for Bool<{ true }> {}

// Placeholder for builtin type.
#[derive(Copy, Clone)]
struct MaxAlignment<const ALIGN: usize>;

#[repr(C)]
pub union RawStorage<const SIZE: usize, const ALIGN: usize> {
    _x: [u8; SIZE],
    _y: MaxAlignment<ALIGN>,
}

impl<T: ?Sized> VTable<T> {
    pub fn new<'a, U>() -> &'a Self
    where U: Unsize<T> {
        &Self {
            cast_ref: |p| p.cast::<U>(),
            cast_mut: |p| p.cast::<U>(),
        }
    }
}

impl<'a, D: ?Sized, T> InlineDyn<'a, D, T> {
    // TODO: Re-enable once lazy normalization has landed.
    // pub fn new<U>(value: U) -> Self
    // where U: Unsize<D> + 'a, Bool<{mem::size_of::<U>() <= mem::size_of::<T>() && mem::align_of::<U>() <= mem::align_of::<T>()}>: Assert {
    //     Self::try_new(value).unwrap()
    // }

    pub fn try_new<U>(value: U) -> Result<Self, U>
    where U: Unsize<D> + 'a {
        let vtable = VTable::new::<U>();
        unsafe { Self::with_vtable(vtable, value) }
    }

    // TODO: Add const generic bound to ensure this function always succeeds.
    #[cfg(feature = "std")]
    pub fn try_or_box<U>(value: U) -> Self
    where
        U: Unsize<D> + 'a,
        Box<U>: Unsize<D> + 'a, {
        Self::try_new(value)
            .or_else(|v| Self::try_new(Box::new(v)))
            .ok()
            .expect("Insufficient space for box")
    }
}
