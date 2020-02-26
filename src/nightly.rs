#![allow(dead_code)]
use core::marker::Unsize;

use crate::{Align, InlineDyn, Size, VTable};

pub struct Bool<const B: bool>;

pub trait Assert {}

impl Assert for Bool<{ true }> {}

impl<D: ?Sized> VTable<D> {
    fn new<'a, T>() -> &'a Self
    where T: Unsize<D> {
        &Self {
            cast_ref: |p| p.cast::<T>(),
            cast_mut: |p| p.cast::<T>(),
        }
    }
}

impl<'a, D, S, A> InlineDyn<'a, D, S, A>
where
    D: ?Sized,
    S: Size,
    A: Align,
{
    // TODO: Re-enable once lazy normalization has landed.
    // pub fn new<U>(value: U) -> Self
    // where U: Unsize<D> + 'a, Bool<{mem::size_of::<U>() <= mem::size_of::<T>() && mem::align_of::<U>() <= mem::align_of::<T>()}>: Assert {
    //     Self::try_new(value).unwrap()
    // }

    pub fn try_new<T>(value: T) -> Result<Self, T>
    where T: Unsize<D> + 'a {
        let vtable = VTable::new::<T>();
        unsafe { Self::with_metadata(vtable, value) }
    }

    // TODO: Add const bound on size/alignment to ensure this function always succeeds.
    #[cfg(feature = "alloc")]
    pub fn try_or_box<T>(value: T) -> Self
    where
        T: Unsize<D> + 'a,
        alloc::boxed::Box<T>: Unsize<D> + 'a,
        S: typenum::IsGreaterOrEqual<crate::DefaultSize>,
        A: typenum::IsGreaterOrEqual<crate::DefaultSize>, {
        Self::try_new(value)
            .or_else(|v| Self::try_new(Box::new(v)))
            .ok()
            .expect("insufficient space for box")
    }
}
