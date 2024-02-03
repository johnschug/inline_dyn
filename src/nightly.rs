use core::ops::CoerceUnsized;

use crate::{pointee, Align, Alignment, InlineDyn};

impl<D, const S: usize, const A: usize> InlineDyn<D, S, A>
where
    D: ?Sized,
    Align<A>: Alignment,
{
    /// Constructs a new [`InlineDyn`] containing the given value.
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
    /// use inline_dyn::fmt::InlineDynDisplay;
    ///
    /// let val = <InlineDynDisplay>::new(42usize);
    /// assert_eq!(val.to_string(), "42");
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

    pub fn try_new<T>(value: T) -> Result<Self, T>
    where
        *const T: CoerceUnsized<*const D>,
    {
        // SAFETY: the metadata is created via
        // [`core::ptr::metadata`](::core::ptr::metadata) which ensures that the
        // metadata is appropriate for the value.
        let metadata = pointee::unsize::<D, _>(&value);
        unsafe { Self::try_with_metadata(metadata, value) }
    }

    #[cfg(feature = "alloc")]
    pub fn try_or_box<T>(value: T) -> Self
    where
        *const T: CoerceUnsized<*const D>,
        *const std_alloc::boxed::Box<T>: CoerceUnsized<*const D>,
    {
        Self::try_new(value).unwrap_or_else(|v| Self::new(std_alloc::boxed::Box::new(v)))
    }

    /// Attempts to contruct a new [`InlineDyn`] by unboxing the given value.
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
    pub fn try_unbox(value: std_alloc::boxed::Box<D>) -> Result<Self, std_alloc::boxed::Box<D>> {
        use core::{
            marker::PhantomData,
            mem::{self, ManuallyDrop, MaybeUninit},
            ptr,
        };

        use crate::RawStorage;

        if RawStorage::<S, A>::is_layout_compatible::<D>(&*value) {
            let metadata = ptr::metadata(&*value);
            let mut storage = RawStorage::<S, A>::new();
            unsafe {
                let size = mem::size_of_val(&*value);
                let value: std_alloc::boxed::Box<ManuallyDrop<D>> = mem::transmute(value);
                ptr::copy_nonoverlapping(
                    ptr::from_ref(&**value).cast::<MaybeUninit<u8>>(),
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
