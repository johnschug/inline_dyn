#![cfg_attr(
    feature = "nightly",
    feature(unsize, coerce_unsized, doc_cfg, ptr_metadata)
)]
#![cfg_attr(not(any(feature = "std", test)), no_std)]
#[cfg(feature = "alloc")]
extern crate alloc;
use core::{
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    marker::PhantomData,
    mem::{self, ManuallyDrop},
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr,
};

use cfg_if::cfg_if;
use static_assertions::{assert_impl_all, assert_not_impl_any};

use self::pointee::Metadata;
use self::storage::{Align, RawStorage};

assert_impl_all!(InlineDyn<dyn Debug + Unpin>: Unpin);
assert_impl_all!(InlineDyn<dyn Debug + Send>: Send);
assert_impl_all!(InlineDyn<dyn Debug + Sync>: Sync);
assert_not_impl_any!(InlineDyn<dyn Debug>: Unpin, Send, Sync);

#[cfg(feature = "nightly")]
#[doc(cfg(feature = "nightly"))]
mod nightly;
mod pointee;
mod storage;

#[cfg(not(feature = "nightly"))]
pub use pointee::Coerce;
pub use storage::{Align, Alignment, DEFAULT_SIZE};

struct AssertSizeAlign<T, const SIZE: usize, const ALIGN: usize>(PhantomData<T>);

impl<T, const SIZE: usize, const ALIGN: usize> AssertSizeAlign<T, SIZE, ALIGN> {
    const OK: () = assert!(
        (mem::size_of::<T>() <= SIZE) && (mem::align_of::<T>() <= ALIGN),
        "size and/or alignment insufficient to store value"
    );
}

struct AssertGrow<const X: usize, const Y: usize>;

impl<const X: usize, const Y: usize> AssertGrow<X, Y> {
    const OK: () = assert!(X >= Y, "new value must not be smaller than old");
}

/// A container type that stores a dynamically-sized type (e.g., a trait object) inline within the
/// container.
///
/// The `S` and `A` generic parameters specify the size and alignment (in bytes) of the internal
/// storage. The default size is the size of a pointer and the default alignment is equivalent to
/// the size.
///
/// # Examples
/// ```
/// use inline_dyn::fmt::InlineDynDebug;
///
/// let val = <InlineDynDebug>::new(42u8);
/// assert_eq!(format!("{:?}", val), "42");
/// ```
///
/// Insufficient size:
/// ```compile_fail
/// # use inline_dyn::fmt::InlineDynDebug;
/// let val = InlineDynDebug<1>::new(42u32);
/// ```
///
/// Insufficient alignment:
/// ```compile_fail
/// # use inline_dyn::fmt::InlineDynDebug;
/// let val = InlineDynDebug<4, 1>::new(42u32);
/// ```
pub struct InlineDyn<D: ?Sized, const S: usize = DEFAULT_SIZE, const A: usize = S>
where
    Align<A>: Alignment,
{
    metadata: Metadata<D>,
    storage: RawStorage<S, A>,
    _marker: PhantomData<D>,
}

impl<D: ?Sized, const S: usize, const A: usize> Drop for InlineDyn<D, S, A>
where
    Align<A>: Alignment,
{
    fn drop(&mut self) {
        unsafe {
            (self.get_mut() as *mut D).drop_in_place();
        }
    }
}

impl<D: ?Sized, const S: usize, const A: usize> InlineDyn<D, S, A>
where
    Align<A>: Alignment,
{
    /// # Safety
    /// The caller must guarantee that the specified implementation of `Coerce` can be soundly used
    /// to coerce references to `value` to references to type `D`.
    #[doc(hidden)]
    #[cfg(not(feature = "nightly"))]
    pub unsafe fn with_unsize<C, T>(value: T) -> Self
    where
        C: Coerce<T, D>,
    {
        let metadata = pointee::unsize::<C, _, T>();
        Self::with_metadata(metadata, value)
    }

    /// # Safety
    /// The caller must guarantee that the provided metadata can be soundly used to convert
    /// references to `value` to references to type `D`.
    unsafe fn with_metadata<T>(metadata: Metadata<D>, val: T) -> Self {
        let () = AssertSizeAlign::<T, S, A>::OK;
        let mut storage = RawStorage::new();
        storage.as_mut_ptr().cast::<T>().write(val);
        Self {
            metadata,
            storage,
            _marker: PhantomData,
        }
    }

    /// Returns a shared reference to the stored value.
    pub fn get_ref(&self) -> &D {
        // SAFETY: the constructors for `InlineDyn` guarantee that storage is always initialized
        // and that `self.metadata` is appropriate for the stored value.
        unsafe { &*pointee::from_raw_parts(self.storage.as_ptr().cast(), self.metadata) }
    }

    /// Returns a mutable reference to the stored value.
    pub fn get_mut(&mut self) -> &mut D {
        // SAFETY: same as `get_ref`.
        unsafe {
            &mut *pointee::from_raw_parts_mut(self.storage.as_mut_ptr().cast(), self.metadata)
        }
    }

    /// Returns a pinned reference to the stored value.
    pub fn get_pinned_mut(self: Pin<&mut Self>) -> Pin<&mut D> {
        // SAFETY: if `self` is pinned then the contained value is also pinned.
        unsafe { self.map_unchecked_mut(|x| x.get_mut()) }
    }

    /// # Safety
    /// The caller must guarantee that the layout of the resulting storage is compatible with
    /// the contained value.
    unsafe fn resize_unchecked<const U: usize, const V: usize>(this: Self) -> InlineDyn<D, U, V>
    where
        Align<V>: Alignment,
    {
        let size = mem::size_of_val(this.get_ref());
        let this = ManuallyDrop::new(this);
        let mut storage = RawStorage::<U, V>::new();
        // SAFETY: the data is non-overlapping and the layout is a precondition.
        ptr::copy_nonoverlapping(this.storage.as_ptr(), storage.as_mut_ptr(), size);
        InlineDyn {
            metadata: this.metadata,
            storage,
            _marker: PhantomData,
        }
    }

    /// Attempts to move the value contained in `this` into a new `InlineDyn`
    /// with the given size (`U`) and alignment (`V`).
    ///
    /// The size and alignment must be large enough to store the contained value, otherwise `this`
    /// is returned.
    ///
    /// # Examples
    /// ```
    /// # use inline_dyn::fmt::InlineDynDisplay;
    /// let val = InlineDynDisplay::new(42u8);
    /// let val: InlineDynDisplay<1> = <InlineDynDisplay>::try_resize(val).ok().unwrap();
    /// assert_eq!(val.to_string(), "42");
    /// ```
    ///
    /// Insufficient size/alignment:
    /// ```
    /// # use inline_dyn::fmt::InlineDynDisplay;
    /// let val = InlineDynDisplay::new(42u32);
    /// let val: Result<InlineDynDisplay<1>, _> = <InlineDynDisplay>::try_resize(val);
    /// assert!(val.is_err());
    /// ```
    pub fn try_resize<const U: usize, const V: usize>(
        this: Self,
    ) -> Result<InlineDyn<D, U, V>, Self>
    where
        Align<V>: Alignment,
    {
        let (size, align) = {
            let val = this.get_ref();
            (mem::size_of_val(val), mem::align_of_val(val))
        };
        if (size <= U) && (align <= mem::align_of::<RawStorage<U, V>>()) {
            // SAFETY: the size and alignment have been checked.
            unsafe { Ok(Self::resize_unchecked(this)) }
        } else {
            Err(this)
        }
    }

    /// Moves the value contained in `this` into a new `InlineDyn` with the given size (`U`),
    /// and alignment (`V`).
    ///
    /// The size and alignment must be at least as large as the current,
    /// otherwise a compiler error is emitted.
    pub fn grow<const U: usize, const V: usize>(this: Self) -> InlineDyn<D, U, V>
    where
        Align<V>: Alignment,
    {
        let () = AssertGrow::<U, S>::OK;
        let () = AssertGrow::<V, A>::OK;
        // SAFETY: the size and alignment have been checked.
        unsafe { Self::resize_unchecked(this) }
    }
}

impl<D: Unpin + ?Sized, const S: usize, const A: usize> Unpin for InlineDyn<D, S, A> where
    Align<A>: Alignment
{
}

impl<D: ?Sized, const S: usize, const A: usize> Deref for InlineDyn<D, S, A>
where
    Align<A>: Alignment,
{
    type Target = D;

    fn deref(&self) -> &Self::Target {
        self.get_ref()
    }
}

impl<D: ?Sized, const S: usize, const A: usize> DerefMut for InlineDyn<D, S, A>
where
    Align<A>: Alignment,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

impl<D: Debug + ?Sized, const S: usize, const A: usize> Debug for InlineDyn<D, S, A>
where
    Align<A>: Alignment,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(self.get_ref(), f)
    }
}

impl<D: Display + ?Sized, const S: usize, const A: usize> Display for InlineDyn<D, S, A>
where
    Align<A>: Alignment,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(self.get_ref(), f)
    }
}

#[macro_export]
macro_rules! dyn_star {
    ($($trait:path),+ $(,)?) => {
        $crate::InlineDyn::<dyn ($($trait)*)>
    };
    ($($trait:path,)+ $l:lifetime $(,)?) => {
        $crate::InlineDyn::<dyn $($trait)* + $l>
    };
}

cfg_if! {
    if #[cfg(feature = "nightly")] {
        /// Constructs a new `InlineDyn` containing the given value.
        ///
        /// The size and alignment of the internal storage must be large enough to
        /// store the given value, otherwise a compiler error is emitted.
        ///
        /// # Examples
        /// ```
        /// use core::fmt::Debug;
        /// use inline_dyn::{fmt::InlineDynDebug, inline_dyn};
        ///
        /// let val = inline_dyn![Debug; 42usize];
        /// assert_eq!(format!("{:?}", val), "42");
        /// ```
        #[macro_export]
        macro_rules! inline_dyn {
            ($trait:path $(: $($_arg:ident),*)?; $e:expr) => {
                $crate::InlineDyn::<(dyn $trait + '_)>::new($e)
            };
        }
    } else {
        /// Constructs a new `InlineDyn` containing the given value.
        ///
        /// The size and alignment of the internal storage must be large enough to
        /// store the given value, otherwise a compiler error is emitted.
        ///
        /// # Examples
        /// ```
        /// use core::fmt::Debug;
        /// use inline_dyn::{fmt::InlineDynDebug, inline_dyn};
        ///
        /// let val: InlineDynDebug = inline_dyn![Debug; 42usize];
        /// assert_eq!(format!("{:?}", val), "42");
        /// ```
        #[macro_export]
        macro_rules! inline_dyn {
            ($trait:path $(: $($arg:ident),*)?; $e:expr) => {{
                fn try_build<'a, $($($arg,)*)* _T, const _S: usize, const _A: usize>(value: _T) -> $crate::InlineDyn<dyn $trait + 'a, _S, _A>
                where _T: $trait + 'a, $crate::Align<_A>: $crate::Alignment, {
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
                try_build($e)
            }};
        }

        impl<T, const S: usize, const A: usize> InlineDyn<[T], S, A>
        where
            Align<A>: Alignment,
        {
            /// Constructs a new `InlineDyn` containing the given value.
            ///
            /// The size and alignment of the internal storage must be large
            /// enough to store the given value, otherwise a compiler error is
            /// emitted.
            ///
            /// # Examples
            /// Insufficient size:
            /// ```compile_fail
            /// InlineDyn::<[u8], 2>::new([1, 2, 3, 4]);
            /// ```
            ///
            /// Insufficient alignment:
            /// ```compile_fail
            /// InlineDyn::<[u32], 16, 2>::new([1, 2, 3, 4]);
            /// ```
            pub fn new<const N: usize>(value: [T; N]) -> Self {
                struct Unsize;
                unsafe impl<_T, const _N: usize> crate::Coerce<[_T; _N], [_T]> for Unsize {
                    fn coerce(p: *const [_T; _N]) -> *const [_T] {
                        p
                    }
                }

                unsafe { Self::with_unsize::<Unsize, _>(value) }
            }
        }
    }
}

macro_rules! impl_new {
    ($trait:path $(, $arg:ident)*) => {
        #[cfg(not(feature = "nightly"))]
        impl<'a, const _S: usize, const _A: usize $(, $arg)*> $crate::InlineDyn<(dyn $trait + 'a), _S, _A>
        where $crate::Align<_A>: $crate::Alignment {
            pub fn new<_T: $trait + 'a>(value: _T) -> Self {
                inline_dyn![$trait: $($arg),*; value]
            }
        }
    };
}

pub mod any {
    use core::{
        any::{Any, TypeId},
        mem::ManuallyDrop,
    };

    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};

    pub type InlineDynAny<const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Any, S, A>;

    #[cfg(not(feature = "nightly"))]
    impl<const S: usize, const A: usize> InlineDynAny<S, A>
    where
        Align<A>: Alignment,
    {
        pub fn new<T: Any>(value: T) -> Self {
            inline_dyn![Any; value]
        }
    }

    impl<D, const S: usize, const A: usize> InlineDyn<D, S, A>
    where
        D: Any + ?Sized,
        Align<A>: Alignment,
    {
        pub fn downcast<T: Any>(self) -> Result<T, Self> {
            if self.get_ref().type_id() == TypeId::of::<T>() {
                let this = ManuallyDrop::new(self);
                // SAFETY: the type id of the stored value has been checked so it is safe to cast
                // and `self` has been wrapped in a `ManuallyDrop`.
                unsafe { Ok((this.get_ref() as *const D).cast::<T>().read()) }
            } else {
                Err(self)
            }
        }
    }
}

pub mod convert {
    use crate::{InlineDyn, DEFAULT_SIZE};

    pub type InlineDynAsRef<'a, U, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn core::convert::AsRef<U> + 'a, S, A>;
    pub type InlineDynAsMut<'a, U, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn core::convert::AsMut<U> + 'a, S, A>;

    impl_new!(AsRef<U>, U);
    impl_new!(AsMut<U>, U);
}

pub mod fmt {
    use crate::{InlineDyn, DEFAULT_SIZE};

    pub type InlineDynDebug<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn core::fmt::Debug + 'a, S, A>;
    pub type InlineDynDisplay<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn core::fmt::Display + 'a, S, A>;

    impl_new!(core::fmt::Debug);
    impl_new!(core::fmt::Display);
}

pub mod future {
    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};
    use core::{
        future::Future,
        pin::Pin,
        task::{Context, Poll},
    };

    pub type InlineDynFuture<'a, O, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Future<Output = O> + 'a, S, A>;

    impl_new!(Future<Output = O>, O);

    impl<F, const S: usize, const A: usize> Future for InlineDyn<F, S, A>
    where
        F: Future + ?Sized,
        Align<A>: Alignment,
    {
        type Output = F::Output;

        fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
            self.get_pinned_mut().poll(cx)
        }
    }
}

pub mod hash {
    use core::hash::Hasher;

    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};

    pub type InlineDynHasher<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Hasher + 'a, S, A>;

    impl<H, const S: usize, const A: usize> Hasher for InlineDyn<H, S, A>
    where
        H: Hasher + ?Sized,
        Align<A>: Alignment,
    {
        fn write(&mut self, bytes: &[u8]) {
            self.get_mut().write(bytes)
        }

        fn finish(&self) -> u64 {
            self.get_ref().finish()
        }
    }
}

pub mod iter {
    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};

    pub type InlineDynIterator<'a, I, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Iterator<Item = I> + 'a, S, A>;
    pub type InlineDynDoubleEndedIterator<
        'a,
        I,
        const S: usize = DEFAULT_SIZE,
        const A: usize = S,
    > = InlineDyn<dyn DoubleEndedIterator<Item = I> + 'a, S, A>;

    impl_new!(Iterator<Item = I>, I);
    impl_new!(DoubleEndedIterator<Item = I>, I);

    impl<I, const S: usize, const A: usize> Iterator for InlineDyn<I, S, A>
    where
        I: Iterator + ?Sized,
        Align<A>: Alignment,
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

    impl<I, const S: usize, const A: usize> DoubleEndedIterator for InlineDyn<I, S, A>
    where
        I: DoubleEndedIterator + ?Sized,
        Align<A>: Alignment,
    {
        fn next_back(&mut self) -> Option<Self::Item> {
            self.get_mut().next_back()
        }

        fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
            self.get_mut().nth_back(n)
        }
    }

    impl<I, const S: usize, const A: usize> ExactSizeIterator for InlineDyn<I, S, A>
    where
        I: ExactSizeIterator + ?Sized,
        Align<A>: Alignment,
    {
        fn len(&self) -> usize {
            self.get_ref().len()
        }
    }

    impl<I, const S: usize, const A: usize> core::iter::FusedIterator for InlineDyn<I, S, A>
    where
        I: core::iter::FusedIterator + ?Sized,
        Align<A>: Alignment,
    {
    }
}

pub mod ops {
    #[macro_export]
    macro_rules! InlineDynFn {
        (($($Args:ty),*) $(-> $R:ty)?) => {
            $crate::InlineDynFn!(($($Args),*) $(-> $R)?; $crate::DEFAULT_SIZE)
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
            $crate::InlineDynFnMut!(($($Args),*) $(-> $R)?; $crate::DEFAULT_SIZE)
        };
        (($($Args:ty),*) $(-> $R:ty)?; $S:ty) => {
            $crate::InlineDyn<dyn FnMut($($Args),*)$(-> $R)?, $S, $S>
        };
        (($($Args:ty),*) $(-> $R:ty)?; $S:ty, $A:ty) => {
            $crate::InlineDyn<dyn FnMut($($Args),*)$(-> $R)?, $S, $A>
        };
    }
}

cfg_if! {
    if #[cfg(feature = "std")] {
        #[cfg_attr(feature = "nightly", doc(cfg(feature = "std")))]
        pub mod error {
            use std::error::Error;

            use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};

            pub type InlineDynError<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> = InlineDyn<dyn Error + 'a, S, A>;

            impl_new!(Error);

            #[allow(deprecated)]
            impl<E, const S: usize, const A: usize> Error for InlineDyn<E, S, A>
            where
                E: Error + ?Sized,
                Align<A>: Alignment,
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

        #[cfg_attr(feature = "nightly", doc(cfg(feature = "std")))]
        pub mod io {
            use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};
            use std::io;

            pub type InlineDynRead<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> = InlineDyn<dyn io::Read + 'a, S, A>;
            pub type InlineDynWrite<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> = InlineDyn<dyn io::Write + 'a, S, A>;
            pub type InlineDynSeek<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> = InlineDyn<dyn io::Seek + 'a, S, A>;
            pub type InlineDynBufRead<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> = InlineDyn<dyn io::BufRead + 'a, S, A>;

            impl_new!(io::Read);
            impl_new!(io::Write);
            impl_new!(io::Seek);
            impl_new!(io::BufRead);

            impl<T, const S: usize, const A: usize> io::Read for InlineDyn<T, S, A>
            where
                T: io::Read + ?Sized,
                Align<A>: Alignment,
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

            impl<T, const S: usize, const A: usize> io::Write for InlineDyn<T, S, A>
            where
                T: io::Write + ?Sized,
                Align<A>: Alignment,
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

            impl<T, const S: usize, const A: usize> io::Seek for InlineDyn<T, S, A>
            where
                T: io::Seek + ?Sized,
                Align<A>: Alignment,
            {
                fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
                    self.get_mut().seek(pos)
                }
            }

            impl<T, const S: usize, const A: usize> io::BufRead for InlineDyn<T, S, A>
            where
                T: io::BufRead + ?Sized,
                Align<A>: Alignment,
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
    use core::cell::Cell;

    use super::{fmt::InlineDynDisplay, *};

    #[test]
    fn test_simple() {
        let val = <dyn_star!(Debug)>::new(42usize);
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
            let dbg = <dyn_star!(Debug, '_)>::new(Dropper(&mut dropped));
            assert_eq!(format!("{:?}", dbg), "Dropper(false)");
        }
        assert_eq!(dropped, true);
    }

    #[test]
    fn test_resize() {
        let val = <dyn_star!(Display)>::new(42u8);
        assert_eq!(val.to_string(), "42");

        let val: InlineDynDisplay<1> = <dyn_star!(Display)>::try_resize(val).ok().unwrap();
        assert_eq!(val.to_string(), "42");
    }

    #[test]
    fn test_resize_insufficient() {
        let val = <dyn_star!(Display)>::new(42u32);
        assert_eq!(val.to_string(), "42");

        let res: Result<InlineDynDisplay<1>, _> = <dyn_star!(Display)>::try_resize(val);
        assert!(res.is_err());
    }

    #[test]
    fn test_slice() {
        let val = InlineDyn::<[u8], 4>::new([1, 2, 3, 4]);
        assert_eq!(val.get_ref(), [1, 2, 3, 4]);
    }

    #[test]
    fn test_interior_mutability() {
        trait Foo {
            fn foo(&self) -> u32;
        }

        struct Bar(Cell<u32>);

        impl Foo for Bar {
            fn foo(&self) -> u32 {
                let r = self.0.get();
                self.0.set(r + 1);
                r
            }
        }

        let val: dyn_star!(Foo) = inline_dyn![Foo; Bar(Cell::new(0))];
        assert_eq!(val.foo(), 0);
        assert_eq!(val.foo(), 1);
        assert_eq!(val.foo(), 2);
    }

    macro_rules! assert_matches {
        ($left:expr, $(|)? $($pattern:pat_param)|+ $(if $guard:expr)? $(,)?) => {
            match $left {
                $($pattern)|+ $(if $guard)? => {}
                ref left_val => {
                    panic!(r#"assertion failed: `(left matches right)`
                     left: `{left_val:?}`
                    right: `{}`"#, stringify!($($pattern)|+ $(if $guard)?))
                }
            }
        };
    }

    #[test]
    fn test_not_unpin() {
        use super::future::InlineDynFuture;
        use core::{
            future::{poll_fn, Future},
            pin::pin,
            ptr,
            task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
        };

        struct NoopWaker;

        impl From<NoopWaker> for RawWaker {
            fn from(_: NoopWaker) -> Self {
                RawWaker::new(
                    ptr::null(),
                    &RawWakerVTable::new(|_| NoopWaker.into(), |_| (), |_| (), |_| ()),
                )
            }
        }

        impl From<NoopWaker> for Waker {
            fn from(value: NoopWaker) -> Self {
                unsafe { Waker::from_raw(value.into()) }
            }
        }

        async fn delay(mut amt: usize) {
            let amt = &mut amt; // ensure returned future is self-referential
            poll_fn(|cx| {
                cx.waker().wake_by_ref();
                match amt {
                    0 => return Poll::Ready(()),
                    _ => {
                        *amt -= 1;
                        return Poll::Pending;
                    }
                }
            })
            .await
        }

        let waker = Waker::from(NoopWaker);
        let mut cx = Context::from_waker(&waker);
        // 1KiB should be enough for anybody
        let mut fut = pin!(<InlineDynFuture<u32, 1024, 16>>::new(async {
            delay(3).await;
            42
        }));
        assert_matches!(fut.as_mut().poll(&mut cx), Poll::Pending);
        assert_matches!(fut.as_mut().poll(&mut cx), Poll::Pending);
        assert_matches!(fut.as_mut().poll(&mut cx), Poll::Pending);
        assert_matches!(fut.as_mut().poll(&mut cx), Poll::Ready(42));
    }
}
