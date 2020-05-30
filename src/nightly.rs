#![allow(dead_code)]
use core::{marker::Unsize, mem};

use crate::{storage::RawStorage, Align, InlineDyn, Size, VTable};

pub struct Predicate<const B: bool>;

pub trait Satisfied {}

impl Satisfied for Predicate<true> {}

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
    pub fn new<T>(value: T) -> Self
    where
        T: Unsize<D> + 'a,
        Predicate<
            {
                mem::size_of::<T>() <= mem::size_of::<RawStorage<S, A>>()
                    && mem::align_of::<T>() <= mem::align_of::<RawStorage<S, A>>()
            },
        >: Satisfied, {
        Self::try_new(value).ok().expect("impossible!")
    }

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

// #[cfg(test)]
// mod tests {
//     use crate::fmt::{InlineDynDebug, InlineDynDisplay};
//
//     #[test]
//     fn test_new() {
//         let val: InlineDynDebug = InlineDynDebug::new(42usize);
//         assert_eq!(format!("{:?}", val.get_ref()), "42");
//     }
// }
