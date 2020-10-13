use core::marker::Unsize;

use crate::{Align, InlineDyn, Size, VTable};

impl<D: ?Sized> VTable<D> {
    fn new<'a, T>() -> &'a Self
    where T: Unsize<D> {
        &Self {
            cast_ref: |p| p.cast::<T>(),
            cast_mut: |p| p.cast::<T>(),
        }
    }
}

// pub struct Predicate<const B: bool>;
//
// pub trait Satisfied {}
//
// impl Satisfied for Predicate<true> {}

impl<'a, D, S, A> InlineDyn<'a, D, S, A>
where
    D: ?Sized,
    S: Size,
    A: Align,
{
    // pub fn new<T>(value: T) -> Self
    // where
    //     T: Unsize<D> + 'a,
    //     Predicate<
    //         {
    //             mem::size_of::<T>() <= mem::size_of::<RawStorage<S, A>>()
    //                 && mem::align_of::<T>() <= mem::align_of::<RawStorage<S, A>>()
    //         },
    //     >: Satisfied, {
    //     Self::try_new(value).ok().expect("impossible!")
    // }

    /// Attempt to construct a new `InlineDyn` containing the given value.
    ///
    /// The size and alignment of the internal storage must be large enough to store
    /// the given value, otherwise the value is returned.
    ///
    /// **Note:** if not using the nightly feature, this method is only implemented for certain
    /// values of `D`. The [`inline_dyn!`] macro can be used as alternative on stable.
    pub fn try_new<T>(value: T) -> Result<Self, T>
    where T: Unsize<D> + 'a {
        let vtable = VTable::new::<T>();
        // SAFETY: the metadata (vtable) is created via `VTable::new` which ensures that the value
        // can be coerced to type `D`.
        unsafe { Self::with_metadata(vtable, value) }
    }

    /// Construct a new `InlineDyn` containing the given value if the value can fit in the internal
    /// storage, or else box the value and store that.
    ///
    /// **Note:** if not using the nightly feature, this method is only implemented for certain
    /// values of `D`. The [`inline_dyn_box!`] macro can be used as alternative on stable.
    ///
    /// # Examples
    /// ```
    /// use core::mem;
    /// use inline::fmt::InlineDynDebug;
    ///
    /// #[derive(Debug)]
    /// struct LargerThanBox([usize; 5]);
    ///
    /// let val: InlineDynDebug = InlineDynDebug::try_or_box(LargerThanBox([1, 2, 3, 4, 5]));
    /// assert!(mem::size_of_val(val.get_ref()) < mem::size_of::<LargerThanBox>());
    /// assert_eq!(format!("{:?}", val.get_ref()), "LargerThanBox([1, 2, 3, 4, 5])");
    /// ```
    #[cfg(feature = "alloc")]
    pub fn try_or_box<T>(value: T) -> Self
    where
        T: Unsize<D> + 'a,
        alloc::boxed::Box<T>: Unsize<D> + 'a,
        S: typenum::IsGreaterOrEqual<crate::DefaultSize>,
        A: typenum::IsGreaterOrEqual<crate::DefaultSize>, {
        Self::try_new(value)
            .or_else(|v| Self::try_new(alloc::boxed::Box::new(v)))
            .ok()
            .expect("insufficient space for box")
    }
}

#[cfg(test)]
mod tests {
    use crate::InlineDyn;
    use typenum::{U16, U2, U4};

    // #[test]
    // fn test_new() {
    //     let val: InlineDynDebug = InlineDynDebug::new(42usize);
    //     assert_eq!(format!("{:?}", val.get_ref()), "42");
    // }

    #[test]
    fn test_slice() {
        let val: InlineDyn<[u8], U4> = InlineDyn::try_new([1, 2, 3, 4]).unwrap();
        assert_eq!(val.get_ref(), [1, 2, 3, 4]);
    }

    #[test]
    fn test_slice_size_insufficient() {
        let res: Result<InlineDyn<[u8], U2>, _> = InlineDyn::try_new([1, 2, 3, 4]);
        assert!(res.is_err());
    }

    #[test]
    fn test_slice_alignment_insufficient() {
        let res: Result<InlineDyn<[u32], U16, U2>, _> = InlineDyn::try_new([1, 2, 3, 4]);
        assert!(res.is_err());
    }
}
