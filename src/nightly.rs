use core::ops::CoerceUnsized;

use crate::{pointee, Align, Alignment, InlineDyn};

impl<D, const S: usize, const A: usize> InlineDyn<D, S, A>
where
    D: ?Sized,
    Align<A>: Alignment,
{
    /// Constructs a new `InlineDyn` containing the given value.
    ///
    /// The size and alignment of the internal storage must be large enough to
    /// store the given value, otherwise a compiler error is emitted.
    ///
    /// **Note:** if not using the nightly feature, this method is only
    /// implemented for certain values of `D`. The
    /// [`inline_dyn!`](crate::inline_dyn!) macro can be used as alternative on
    /// stable.
    ///
    /// # Examples
    /// ```
    /// use inline_dyn::fmt::InlineDynDebug;
    ///
    /// let val = <InlineDynDebug>::new(42usize);
    /// assert_eq!(format!("{:?}", val), "42");
    /// ```
    pub fn new<T>(value: T) -> Self
    where
        *const T: CoerceUnsized<*const D>,
    {
        // SAFETY: the metadata is created via
        // [`core::ptr::metadata`](::core::ptr::metadata) which ensures that the
        // metadata is appropriate for the value.
        let metadata = pointee::unsize::<D, _>(&value);
        unsafe { Self::with_metadata(metadata, value) }
    }

    /// Attempts to contruct a new `InlineDyn` by unboxing the given value.
    ///
    /// The size and alignment of the internal storage must be large enough to
    /// store the given value, otherwise the boxed value is returned.
    ///
    /// # Examples
    /// ```
    /// use inline_dyn::InlineDyn;
    ///
    /// let val = <InlineDyn<[u8], 4, 1>>::try_unbox(Box::new([0u8, 1, 2, 3])).ok().unwrap();
    /// assert_eq!(val.get_ref(), &[0, 1, 2, 3]);
    /// ```
    #[cfg(feature = "alloc")]
    #[doc(cfg(feature = "alloc"))]
    pub fn try_unbox(value: alloc::boxed::Box<D>) -> Result<Self, alloc::boxed::Box<D>> {
        use core::{
            marker::PhantomData,
            mem::{self, ManuallyDrop, MaybeUninit},
            ptr,
        };

        use crate::RawStorage;

        let (size, align) = { (mem::size_of_val(&*value), mem::align_of_val(&*value)) };
        if (size <= S) && (align <= mem::align_of::<RawStorage<S, A>>()) {
            let metadata = ptr::metadata(&*value);
            let mut storage = RawStorage::<S, A>::new();
            unsafe {
                let value: Box<ManuallyDrop<D>> = mem::transmute(value);
                ptr::copy_nonoverlapping(
                    (&**value as *const D).cast::<MaybeUninit<u8>>(),
                    storage.as_mut_ptr(),
                    size,
                );
            }
            Ok(Self {
                metadata,
                storage,
                _marker: PhantomData,
            })
        } else {
            Err(value)
        }
    }
}
