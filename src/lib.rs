#![cfg_attr(feature = "nightly", allow(incomplete_features))]
#![cfg_attr(feature = "nightly", feature(unsize, const_generics))]
#![cfg_attr(not(any(feature = "std", test)), no_std)]
#[cfg(feature = "alloc")]
extern crate alloc;
use core::{
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    marker::PhantomData,
    mem::{self, ManuallyDrop},
    ops::{Deref, DerefMut},
    ptr,
};

use cfg_if::cfg_if;
use static_assertions::{assert_impl_all, assert_not_impl_any};

use self::storage::RawStorage;

assert_impl_all!(InlineDyn<'static, dyn Debug + Unpin>: Unpin);
assert_impl_all!(InlineDyn<'static, dyn Debug + Send>: Send);
assert_impl_all!(InlineDyn<'static, dyn Debug + Sync>: Sync);
assert_not_impl_any!(InlineDyn<'static, dyn Debug>: Unpin, Send, Sync);

#[cfg(feature = "nightly")]
mod nightly;
mod storage;

pub use storage::{Align, Size};

/// A type-level integer corresponding to the default [size](Size) and [alignment](Align) of the
/// internal storage of an [`InlineDyn`].
///
/// The value of this integer should be equivalent to the size of a pointer (in bytes).
pub type DefaultSize = storage::DefaultSize;

#[doc(hidden)]
pub unsafe trait Coerce<T, U: ?Sized> {
    fn coerce(p: *const T) -> *const U;
}

struct VTable<D: ?Sized> {
    cast_ref: unsafe fn(*const ()) -> *const D,
    cast_mut: unsafe fn(*mut ()) -> *mut D,
}

impl<D: ?Sized> VTable<D> {
    unsafe fn with_unsize<'a, C, T: 'a>() -> &'a Self
    where C: Coerce<T, D> {
        &Self {
            cast_ref: |p| C::coerce(p.cast::<T>()),
            cast_mut: |p| C::coerce(p.cast::<T>()) as *mut _,
        }
    }
}

/// A container type that stores a dynamically-sized type (e.g., a trait object) inline within the
/// container.
///
/// The `S` and `A` generic parameters specify the size and alignment (in bytes) of the internal
/// storage.
///
/// # Examples
/// ```
/// use inline::fmt::InlineDynDebug;
///
/// let val: InlineDynDebug = InlineDynDebug::try_new(42u8).unwrap();
/// assert_eq!(format!("{:?}", val), "42");
/// ```
pub struct InlineDyn<'a, D: ?Sized + 'a, S: Size = DefaultSize, A: Align = S> {
    // TODO: Replace with DynMetadata if RFC 2580 is accepted.
    metadata: &'a VTable<D>,
    storage: RawStorage<S, A>,
    _marker: PhantomData<D>,
}

impl<D: ?Sized, S: Size, A: Align> Drop for InlineDyn<'_, D, S, A> {
    fn drop(&mut self) {
        unsafe {
            (self.get_mut() as *mut D).drop_in_place();
        }
    }
}

impl<'a, D: ?Sized, S: Size, A: Align> InlineDyn<'a, D, S, A> {
    /// # Safety
    /// The caller must guarantee that the specified implementation of `Coerce` can be soundly used
    /// to convert references to `value` to references to type `D`.
    #[doc(hidden)]
    pub unsafe fn with_unsize<C, T: 'a>(value: T) -> Result<Self, T>
    where C: Coerce<T, D> {
        Self::with_metadata(VTable::with_unsize::<C, T>(), value)
    }

    /// # Safety
    /// The caller must guarantee that the provided metadata can be soundly used to convert
    /// references to `value` to references to type `D`.
    unsafe fn with_metadata<T: 'a>(metadata: &'a VTable<D>, value: T) -> Result<Self, T> {
        if (mem::size_of_val(&value) <= mem::size_of::<RawStorage<S, A>>())
            && (mem::align_of_val(&value) <= mem::align_of::<RawStorage<S, A>>())
        {
            let mut storage = RawStorage::new();
            storage.as_mut_ptr().cast::<T>().write(value);
            Ok(Self {
                metadata,
                storage,
                _marker: PhantomData,
            })
        } else {
            Err(value)
        }
    }

    /// Returns a shared reference to the stored value.
    pub fn get_ref(&self) -> &D {
        // SAFETY: the constructors for `InlineDyn` guarantee that storage is always initialized
        // and that `self.metadata` has appropriate conversion functions for the stored value.
        unsafe { &*(self.metadata.cast_ref)(self.storage.as_ptr().cast()) }
    }

    /// Returns a mutable reference to the stored value.
    pub fn get_mut(&mut self) -> &mut D {
        // SAFETY: same as `get_ref`.
        unsafe { &mut *(self.metadata.cast_mut)(self.storage.as_mut_ptr().cast()) }
    }

    /// Creates a new `InlineDyn` with a larger internal storage containing the value stored in
    /// `this`.
    pub fn grow<U, V>(this: Self) -> InlineDyn<'a, D, U, V>
    where
        U: Size + typenum::IsGreaterOrEqual<S>,
        V: Align + typenum::IsGreaterOrEqual<A>, {
        let (size, align) = {
            let val = this.get_ref();
            (mem::size_of_val(val), mem::align_of_val(val))
        };
        debug_assert!(size <= mem::size_of::<RawStorage<U, V>>());
        debug_assert!(align <= mem::align_of::<RawStorage<U, V>>());

        let this = ManuallyDrop::new(this);
        let mut storage = RawStorage::<U, V>::new();
        // SAFETY: the data is non-overlapping and the bounds for the function guarantee the layout
        // is correct.
        unsafe {
            ptr::copy_nonoverlapping(this.storage.as_ptr(), storage.as_mut_ptr(), size);
        }
        InlineDyn {
            metadata: this.metadata,
            storage,
            _marker: PhantomData,
        }
    }

    /// Attempts to create a new `InlineDyn` with the given size and alignment for the internal
    /// storage containing the value stored in `this`.
    ///
    /// The size and alignment must be large enough to store the contained value, otherwise `this`
    /// is returned.
    pub fn try_resize<U, V>(this: Self) -> Result<InlineDyn<'a, D, U, V>, Self>
    where
        U: Size,
        V: Align, {
        let (size, align) = {
            let val = this.get_ref();
            (mem::size_of_val(val), mem::align_of_val(val))
        };
        if (size <= mem::size_of::<RawStorage<U, V>>())
            && (align <= mem::align_of::<RawStorage<U, V>>())
        {
            let this = ManuallyDrop::new(this);
            let mut storage = RawStorage::<U, V>::new();
            // SAFETY: the data is non-overlapping and the layout has been checked.
            unsafe {
                ptr::copy_nonoverlapping(this.storage.as_ptr(), storage.as_mut_ptr(), size);
            }
            Ok(InlineDyn {
                metadata: this.metadata,
                storage,
                _marker: PhantomData,
            })
        } else {
            Err(this)
        }
    }

    // Requires RFC 2580 or some other way to soundly access DST metadata.
    // #[cfg(feature = "alloc")]
    // pub fn try_unbox(value: alloc::boxed::Box<D>) -> Result<Self, alloc::boxed::Box<D>> {
    //     let (size, align) = {
    //         (mem::size_of_val(&*value), mem::align_of_val(&*value))
    //     };
    //     if (size <= mem::size_of::<T>()) && (align <= mem::align_of::<T>()) {
    //         let metadata = ptr::metadata(&*value);
    //         let mut storage = RawStorage::<U, V>::new();
    //         let value = ManuallyDrop::new(value); // TODO: fix memory leak here
    //         unsafe {
    //             ptr::copy_nonoverlapping(
    //                 (&**value as *const D).cast::<MaybeUninit<u8>>(),
    //                 storage.as_mut_ptr(),
    //                 size,
    //             );
    //         }
    //         Ok(Self {
    //             metadata,
    //             storage,
    //             _marker: PhantomData,
    //         })
    //     } else {
    //         Err(value)
    //     }
    // }
}

impl<D: ?Sized, S: Size, A: Align> Deref for InlineDyn<'_, D, S, A> {
    type Target = D;

    fn deref(&self) -> &Self::Target {
        self.get_ref()
    }
}

impl<D: ?Sized, S: Size, A: Align> DerefMut for InlineDyn<'_, D, S, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

impl<D: Debug + ?Sized, S: Size, A: Align> Debug for InlineDyn<'_, D, S, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(self.get_ref(), f)
    }
}

impl<D: Display + ?Sized, S: Size, A: Align> Display for InlineDyn<'_, D, S, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(self.get_ref(), f)
    }
}

cfg_if! {
    if #[cfg(feature = "nightly")] {
        /// Attempt to construct a new [`InlineDyn`] for the specified trait containing the given value.
        ///
        /// # Examples
        /// ```
        /// use core::fmt::Debug;
        /// use inline::{fmt::InlineDynDebug, inline_dyn};
        ///
        /// let val: InlineDynDebug = inline_dyn![Debug; 42usize].unwrap();
        /// assert_eq!(format!("{:?}", val), "42");
        /// ```
        #[macro_export]
        macro_rules! inline_dyn {
            ($trait:path $(: $($_arg:ident),*)?; $e:expr) => {{
                $crate::InlineDyn::<'_, dyn $trait, _, _>::try_new($e)
            }};
        }
        /// Construct a new [`InlineDyn`] for the specified trait containing the given value if the value
        /// can fit in the internal storage, or else box the value and store that.
        ///
        /// # Examples
        /// ```
        /// use core::fmt::Debug;
        /// use inline::{fmt::InlineDynDebug, inline_dyn_box};
        ///
        /// #[derive(Debug)]
        /// struct LargerThanBox([usize; 5]);
        ///
        /// let val: InlineDynDebug = inline_dyn_box![Debug; LargerThanBox([1, 2, 3, 4, 5])];
        /// assert_eq!(format!("{:?}", val), "LargerThanBox([1, 2, 3, 4, 5])");
        /// ```
        #[macro_export]
        #[cfg(feature = "alloc")]
        macro_rules! inline_dyn_box {
            ($trait:path $(: $($_arg:ident),*)?; $e:expr) => {{
                $crate::InlineDyn::<'_, dyn $trait>::try_or_box($e)
            }};
        }
    } else {
        #[macro_export]
        macro_rules! inline_dyn {
            ($trait:path $(: $($arg:ident),*)?; $e:expr) => {{
                fn try_build<'a, $($($arg,)*)* _T, _S, _A>(value: _T) -> Result<$crate::InlineDyn<'a, dyn $trait + 'a, _S, _A>, _T>
                    where _T: $trait + 'a, _S: $crate::Size, _A: $crate::Align, {
                        struct Unsize;
                        unsafe impl<'a, $($($arg,)*)* _T: $trait + 'a> $crate::Coerce<_T, (dyn $trait + 'a)> for Unsize {
                            fn coerce(p: *const _T) -> *const (dyn $trait + 'a) {
                                p
                            }
                        }

                        unsafe {
                            $crate::InlineDyn::with_unsize::<Unsize, _>(value)
                        }
                    }
                try_build::<$($($arg,)*)* _, _, _>($e)
            }};
        }

        #[macro_export]
        #[cfg(feature = "alloc")]
        macro_rules! inline_dyn_box {
            ($trait:path $(: $($arg:ident),*)?; $e:expr) => {{
                fn build<'a, $($($arg,)*)* _T>(value: _T) -> $crate::InlineDyn<'a, dyn $trait + 'a>
                    where _T: $trait + 'a, alloc::boxed::Box<_T>: $trait + 'a {
                        struct Unsize;
                        unsafe impl<'a, $($($arg,)*)* _T: $trait + 'a> $crate::Coerce<_T, (dyn $trait + 'a)> for Unsize {
                            fn coerce(p: *const _T) -> *const (dyn $trait + 'a) {
                                p
                            }
                        }

                        unsafe {
                            $crate::InlineDyn::with_unsize::<Unsize, _>(value)
                                .or_else(|v| $crate::InlineDyn::with_unsize::<Unsize, _>(alloc::boxed::Box::new(v)))
                                .ok()
                                .expect("insufficient space for box")
                        }
                    }
                build::<$($($arg,)*)* _>($e)
            }};
        }
    }
}

macro_rules! impl_try_new {
    ($trait:path $(, $arg:ident)*) => {
        #[cfg(not(feature = "nightly"))]
        impl<'a, _S: crate::Size, _A: crate::Align $(, $arg)*> crate::InlineDyn<'a, dyn $trait, _S, _A> {
            pub fn try_new<_T: $trait + 'a>(value: _T) -> Result<Self, _T> {
                inline_dyn![$trait: $($arg),*; value]
            }
        }

        #[cfg(all(not(feature = "nightly"), feature = "alloc"))]
        impl<'a $(, $arg)*> crate::InlineDyn<'a, dyn $trait> {
            pub fn try_or_box<_T: $trait + 'a>(value: _T) -> Self
            where alloc::boxed::Box<_T>: $trait + 'a {
                inline_dyn_box![$trait: $($arg),*; value]
            }
        }
    };
}

pub mod any {
    use core::{
        any::{Any, TypeId},
        mem::ManuallyDrop,
    };

    use crate::{Align, DefaultSize, Size};

    pub type InlineDynAny<S = DefaultSize, A = S> = crate::InlineDyn<'static, dyn Any, S, A>;

    #[cfg(not(feature = "nightly"))]
    impl<S: Size, A: Align> InlineDynAny<S, A> {
        pub fn try_new<T: Any>(value: T) -> Result<Self, T> {
            inline_dyn![Any; value]
        }
    }

    impl<D, S, A> crate::InlineDyn<'static, D, S, A>
    where
        D: Any + ?Sized,
        S: Size,
        A: Align,
    {
        pub fn downcast<T: Any>(self) -> Result<T, Self> {
            if self.get_ref().type_id() == TypeId::of::<T>() {
                let this = ManuallyDrop::new(self);
                // SAFETY: the type id of the stored value has been checked so it is safe to cast
                // and `self` has been wrapped in a `ManuallyDrop` and will not be freed so it is
                // safe to read the value out of the pointer.
                unsafe { Ok((this.get_ref() as *const D).cast::<T>().read()) }
            } else {
                Err(self)
            }
        }
    }
}

pub mod convert {
    use crate::DefaultSize;

    pub type InlineDynAsRef<'a, U, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn core::convert::AsRef<U>, S, A>;
    pub type InlineDynAsMut<'a, U, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn core::convert::AsMut<U>, S, A>;

    impl_try_new!(AsRef<U>, U);
    impl_try_new!(AsMut<U>, U);
}

pub mod fmt {
    use crate::DefaultSize;

    pub type InlineDynDebug<'a, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn core::fmt::Debug, S, A>;
    pub type InlineDynDisplay<'a, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn core::fmt::Display, S, A>;

    impl_try_new!(core::fmt::Debug);
    impl_try_new!(core::fmt::Display);
}

pub mod future {
    use crate::{Align, DefaultSize, InlineDyn, Size};
    use core::{
        future::Future,
        pin::Pin,
        task::{Context, Poll},
    };

    pub type InlineDynFuture<'a, O, S = DefaultSize, A = S> =
        InlineDyn<'a, dyn Future<Output = O>, S, A>;

    impl_try_new!(Future<Output = O>, O);

    impl<F, S, A> Future for InlineDyn<'_, F, S, A>
    where
        F: Future + Unpin + ?Sized,
        S: Size,
        A: Align,
    {
        type Output = F::Output;

        fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
            Pin::new(self.get_mut()).poll(cx)
        }
    }
}

pub mod hash {
    pub type InlineDynHasher<'a, S = crate::DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn core::hash::Hasher, S, A>;
}

pub mod iter {
    use crate::{Align, DefaultSize, InlineDyn, Size};

    pub type InlineDynIterator<'a, I, S = DefaultSize, A = S> =
        InlineDyn<'a, dyn Iterator<Item = I>, S, A>;
    pub type InlineDynDoubleEndedIterator<'a, I, S = DefaultSize, A = S> =
        InlineDyn<'a, dyn DoubleEndedIterator<Item = I>, S, A>;

    impl_try_new!(Iterator<Item = I>, I);
    impl_try_new!(DoubleEndedIterator<Item = I>, I);

    impl<I, S, A> Iterator for InlineDyn<'_, I, S, A>
    where
        I: Iterator + ?Sized,
        S: Size,
        A: Align,
    {
        type Item = I::Item;

        fn next(&mut self) -> Option<Self::Item> {
            self.get_mut().next()
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            self.get_ref().size_hint()
        }

        fn nth(&mut self, n: usize) -> Option<Self::Item> {
            self.get_mut().nth(n)
        }
    }

    impl<I, S, A> DoubleEndedIterator for InlineDyn<'_, I, S, A>
    where
        I: DoubleEndedIterator + ?Sized,
        S: Size,
        A: Align,
    {
        fn next_back(&mut self) -> Option<Self::Item> {
            self.get_mut().next_back()
        }

        fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
            self.get_mut().nth_back(n)
        }
    }

    impl<I, S, A> ExactSizeIterator for InlineDyn<'_, I, S, A>
    where
        I: ExactSizeIterator + ?Sized,
        S: Size,
        A: Align,
    {
        fn len(&self) -> usize {
            self.get_ref().len()
        }
    }

    impl<I, S, A> core::iter::FusedIterator for InlineDyn<'_, I, S, A>
    where
        I: core::iter::FusedIterator + ?Sized,
        S: Size,
        A: Align,
    {
    }
}

pub mod ops {
    #[macro_export]
    macro_rules! InlineDynFn {
        (($($Args:ty),*) $(-> $R:ty)?) => {
            $crate::InlineDynFn!(($($Args),*) $(-> $R)?; $crate::DefaultSize)
        };
        (($($Args:ty),*) $(-> $R:ty)?; $S:ty) => {
            $crate::InlineDynFn!(($($Args),*) $(-> $R)?; $S, $S)
        };
        (($($Args:ty),*) $(-> $R:ty)?; $S:ty, $A:ty) => {
            $crate::InlineDyn<dyn Fn($($Args),*)$(-> $R)?, $S, $A>
        };
    }

    #[macro_export]
    macro_rules! InlineDynFnMut {
        (($($Args:ty),*) $(-> $R:ty)?) => {
            $crate::InlineDynFnMut!(($($Args),*) $(-> $R)?; $crate::DefaultSize)
        };
        (($($Args:ty),*) $(-> $R:ty)?; $S:ty) => {
            $crate::InlineDyn<'_, dyn FnMut($($Args),*)$(-> $R)?, $S, $S>
        };
        (($($Args:ty),*) $(-> $R:ty)?; $S:ty, $A:ty) => {
            $crate::InlineDyn<'_, dyn FnMut($($Args),*)$(-> $R)?, $S, $A>
        };
    }
}

cfg_if! {
    if #[cfg(feature = "std")] {
        pub mod error {
            use std::error::Error;

            use crate::{Align, InlineDyn, Size};

            pub type InlineDynError<'a, S = crate::DefaultSize, A = S> = InlineDyn<'a, dyn Error, S, A>;

            impl_try_new!(Error);

            #[allow(deprecated)]
            impl<E, S, A> Error for InlineDyn<'_, E, S, A>
            where
                E: Error + ?Sized,
                S: Size,
                A: Align,
            {
                fn description(&self) -> &str {
                    self.get_ref().description()
                }

                fn cause(&self) -> Option<&dyn Error> {
                    self.get_ref().cause()
                }

                fn source(&self) -> Option<&(dyn Error + 'static)> {
                    self.get_ref().source()
                }
            }
        }

        pub mod io {
            use crate::{Align, DefaultSize, InlineDyn, Size};
            use std::io;

            pub type InlineDynRead<'a, S = DefaultSize, A = S> = InlineDyn<'a, dyn io::Read, S, A>;
            pub type InlineDynWrite<'a, S = DefaultSize, A = S> = InlineDyn<'a, dyn io::Write, S, A>;
            pub type InlineDynSeek<'a, S = DefaultSize, A = S> = InlineDyn<'a, dyn io::Seek, S, A>;
            pub type InlineDynBufRead<'a, S = DefaultSize, A = S> = InlineDyn<'a, dyn io::BufRead, S, A>;

            impl_try_new!(io::Read);
            impl_try_new!(io::Write);
            impl_try_new!(io::Seek);
            impl_try_new!(io::BufRead);

            impl<T, S, A> io::Read for InlineDyn<'_, T, S, A>
            where
                T: io::Read + ?Sized,
                S: Size,
                A: Align,
            {
                fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                    self.get_mut().read(buf)
                }

                fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
                    self.get_mut().read_vectored(bufs)
                }

                fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
                    self.get_mut().read_exact(buf)
                }
            }

            impl<T, S, A> io::Write for InlineDyn<'_, T, S, A>
            where
                T: io::Write + ?Sized,
                S: Size,
                A: Align,
            {
                fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
                    self.get_mut().write(buf)
                }

                fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
                    self.get_mut().write_vectored(bufs)
                }

                fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
                    self.get_mut().write_all(buf)
                }

                fn flush(&mut self) -> io::Result<()> {
                    self.get_mut().flush()
                }
            }

            impl<T, S, A> io::Seek for InlineDyn<'_, T, S, A>
            where
                T: io::Seek + ?Sized,
                S: Size,
                A: Align,
            {
                fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
                    self.get_mut().seek(pos)
                }
            }

            impl<T, S, A> io::BufRead for InlineDyn<'_, T, S, A>
            where
                T: io::BufRead + ?Sized,
                S: Size,
                A: Align,
            {
                fn fill_buf(&mut self) -> io::Result<&[u8]> {
                    self.get_mut().fill_buf()
                }

                fn consume(&mut self, amt: usize) {
                    self.get_mut().consume(amt)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use typenum::{U0, U1, U4};

    use super::*;
    use crate::fmt::{InlineDynDebug, InlineDynDisplay};

    #[test]
    fn test_simple() {
        let val: InlineDynDebug = InlineDynDebug::try_new(42usize).unwrap();
        assert_eq!(format!("{:?}", val.get_ref()), "42");
    }

    #[test]
    fn test_drop() {
        #[derive(Debug)]
        struct Dropper<'a>(&'a mut bool);

        impl Drop for Dropper<'_> {
            fn drop(&mut self) {
                *self.0 = true;
            }
        }

        let mut dropped = false;
        {
            let dbg: InlineDynDebug = InlineDynDebug::try_new(Dropper(&mut dropped)).unwrap();
            assert_eq!(format!("{:?}", dbg), "Dropper(false)");
        }
        assert_eq!(dropped, true);
    }

    #[test]
    fn test_size_insufficient() {
        let res: Result<InlineDynDebug<U0>, _> = InlineDynDebug::try_new(42u8);
        assert!(res.is_err());
    }

    #[test]
    fn test_alignment_insufficient() {
        let res: Result<InlineDynDebug<U4, U1>, _> = InlineDynDebug::try_new(42u32);
        assert!(res.is_err());
    }

    #[test]
    fn test_resize() {
        let val: InlineDynDisplay = InlineDynDisplay::try_new(42u8).unwrap();
        assert_eq!(val.to_string(), "42");

        let val: InlineDynDisplay<U1> = InlineDynDisplay::try_resize(val).ok().unwrap();
        assert_eq!(val.to_string(), "42");
    }

    #[test]
    fn test_resize_insufficient() {
        let val: InlineDynDisplay = InlineDynDisplay::try_new(42u32).unwrap();
        assert_eq!(val.to_string(), "42");

        let res: Result<InlineDynDisplay<U1>, _> = InlineDynDisplay::try_resize(val);
        assert!(res.is_err());
    }

    #[test]
    #[cfg(feature = "alloc")]
    fn test_try_or_box() {
        use std::mem;

        #[derive(Debug)]
        struct LargerThanBox([usize; 5]);
        assert!(mem::size_of::<LargerThanBox>() > mem::size_of::<InlineDynDebug>());

        let val: InlineDynDebug = InlineDynDebug::try_or_box(LargerThanBox([1, 2, 3, 4, 5]));
        assert!(mem::size_of_val(val.get_ref()) < mem::size_of::<LargerThanBox>());
        assert_eq!(
            format!("{:?}", val.get_ref()),
            "LargerThanBox([1, 2, 3, 4, 5])"
        );
    }
}
