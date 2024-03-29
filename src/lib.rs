//! InlineDyn
#![allow(clippy::let_unit_value)]
#![cfg_attr(
    feature = "nightly",
    feature(
        async_iterator,
        coerce_unsized,
        doc_auto_cfg,
        error_generic_member_access,
        error_in_core,
        iter_advance_by,
        ptr_metadata,
        unsize,
    )
)]
#![cfg_attr(all(feature = "nightly", feature = "alloc"), feature(allocator_api))]
#![cfg_attr(all(feature = "nightly", feature = "std"), feature(can_vector))]
#![cfg_attr(not(any(feature = "std", test)), no_std)]
#[cfg(feature = "alloc")]
extern crate alloc as std_alloc;
use core::{
    fmt::Debug,
    iter::FusedIterator,
    marker::PhantomData,
    mem::{self, ManuallyDrop, MaybeUninit},
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr, slice,
};

use cfg_if::cfg_if;
use static_assertions::{assert_impl_all, assert_not_impl_any};

use self::pointee::Metadata;
use self::storage::RawStorage;

assert_impl_all!(InlineDyn<dyn Debug + Unpin>: Unpin);
assert_impl_all!(InlineDyn<dyn Debug + Send>: Send);
assert_impl_all!(InlineDyn<dyn Debug + Sync>: Sync);
assert_not_impl_any!(InlineDyn<dyn Debug>: Unpin, Send, Sync);

#[cfg(feature = "nightly")]
mod nightly;
mod pointee;
mod storage;

pub use storage::{Align, Alignment, DEFAULT_SIZE};

struct AssertLayoutCompatible<T, const SIZE: usize, const ALIGN: usize>(PhantomData<T>);

impl<T, const SIZE: usize, const ALIGN: usize> AssertLayoutCompatible<T, SIZE, ALIGN> {
    const OK: () = assert!(
        (mem::size_of::<T>() <= SIZE) && (mem::align_of::<T>() <= ALIGN),
        "size and/or alignment insufficient to store value"
    );
}

struct AssertLarger<const X: usize, const Y: usize>;

impl<const X: usize, const Y: usize> AssertLarger<X, Y> {
    const OK: () = assert!(X >= Y, "new value must not be smaller than old");
}

/// A container type that stores a dynamically-sized type (e.g., a trait object)
/// inline within the container.
///
/// The `S` and `A` generic parameters specify the size and alignment (in bytes)
/// of the internal storage. The default size is the size of a pointer and the
/// alignment defaults to the specified size.
///
/// # Examples
/// ```
/// use inline_dyn::fmt::InlineDynDisplay;
///
/// let val = <InlineDynDisplay>::new(42u8);
/// assert_eq!(val.to_string(), "42");
/// ```
///
/// Insufficient size:
/// ```compile_fail
/// # use inline_dyn::fmt::InlineDynDisplay;
/// let val = InlineDynDisplay<1>::new(42u32);
/// ```
///
/// Insufficient alignment:
/// ```compile_fail
/// # use inline_dyn::fmt::InlineDynDisplay;
/// let val = InlineDynDisplay<4, 1>::new(42u32);
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
    /// The caller must guarantee that the provided function returns a pointer
    /// to the specified value, which is a valid instance of type `D`.
    #[doc(hidden)]
    #[cfg(not(feature = "nightly"))]
    pub unsafe fn with_cast<T>(value: T, cast: fn(*const T) -> *const D) -> Self {
        let metadata = Metadata::new(unsafe { mem::transmute(cast) });
        unsafe { Self::with_metadata(metadata, value) }
    }

    /// # Safety
    /// The caller must guarantee that the provided function returns a pointer
    /// to the specified value, which is a valid instance of type `D`.
    #[doc(hidden)]
    #[cfg(not(feature = "nightly"))]
    pub unsafe fn try_with_cast<T>(value: T, cast: fn(*const T) -> *const D) -> Result<Self, T> {
        let metadata = Metadata::new(unsafe { mem::transmute(cast) });
        unsafe { Self::try_with_metadata(metadata, value) }
    }

    /// # Safety
    /// The caller must guarantee that the provided metadata can be soundly used
    /// to convert references to `value` to references to type `D`.
    unsafe fn with_metadata<T>(metadata: Metadata<D>, val: T) -> Self {
        let () = AssertLayoutCompatible::<T, S, A>::OK;
        // SAFETY: the layout has been checked and the validity of the metadata
        // is a precondition of the function.
        unsafe { Self::with_metadata_unchecked(metadata, val) }
    }

    /// # Safety
    /// The caller must guarantee that the provided metadata can be soundly used
    /// to convert references to `value` to references to type `D`.
    unsafe fn try_with_metadata<T>(metadata: Metadata<D>, val: T) -> Result<Self, T> {
        if RawStorage::<S, A>::is_layout_compatible::<T>(&val) {
            // SAFETY: the layout has been checked and the validity of the metadata
            // is a precondition of the function.
            unsafe { Ok(Self::with_metadata_unchecked(metadata, val)) }
        } else {
            Err(val)
        }
    }

    /// # Safety
    /// The caller must guarantee that the provided metadata can be soundly used
    /// to convert references to `value` to references to type `D` and that
    /// the internal storage is layout compatiple with type `T`.
    unsafe fn with_metadata_unchecked<T>(metadata: Metadata<D>, val: T) -> Self {
        let mut storage = RawStorage::new();
        // SAFETY: the layout of the storage being sufficient to write a
        // value of type `T` to is a precondition of the function.
        unsafe {
            storage.as_mut_ptr().cast::<T>().write(val);
        }
        Self {
            metadata,
            storage,
            _marker: PhantomData,
        }
    }

    /// Returns a shared reference to the stored value.
    pub fn get_ref(&self) -> &D {
        // SAFETY: the constructors for `InlineDyn` guarantee that storage is
        // always initialized and that `self.metadata` is appropriate for the
        // stored value.
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
    /// The caller must guarantee that the layout of the resulting storage is
    /// compatible with the contained value.
    unsafe fn resize_unchecked<const U: usize, const V: usize>(this: Self) -> InlineDyn<D, U, V>
    where
        Align<V>: Alignment,
    {
        let size = mem::size_of_val(this.get_ref());
        let this = ManuallyDrop::new(this);
        let mut storage = RawStorage::<U, V>::new();
        // SAFETY: the data is non-overlapping and the layout is a precondition.
        unsafe {
            this.storage
                .as_ptr()
                .copy_to_nonoverlapping(storage.as_mut_ptr(), size);
        }
        InlineDyn {
            metadata: this.metadata,
            storage,
            _marker: PhantomData,
        }
    }

    /// Attempts to move the value contained in `this` into a new [`InlineDyn`]
    /// with the given size (`U`) and alignment (`V`).
    ///
    /// The size and alignment must be large enough to store the contained
    /// value, otherwise `this` is returned.
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
        if RawStorage::<U, V>::is_layout_compatible::<D>(this.get_ref()) {
            // SAFETY: the layout has been checked.
            unsafe { Ok(Self::resize_unchecked(this)) }
        } else {
            Err(this)
        }
    }

    /// Moves the value contained in `this` into a new [`InlineDyn`] with the
    /// given size (`U`), and alignment (`V`).
    ///
    /// The size and alignment must be at least as large as the current,
    /// otherwise a compiler error is emitted.
    pub fn grow<const U: usize, const V: usize>(this: Self) -> InlineDyn<D, U, V>
    where
        Align<V>: Alignment,
    {
        let () = AssertLarger::<U, S>::OK;
        let () = AssertLarger::<V, A>::OK;
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

/// A trait for cloning trait objects dynamically.
///
/// # Safety
///
/// An implementation must ensure that a valid value of the same type as the
/// implementor is written to the storage passed to `dyn_clone_into`.
///
/// # Examples
///
/// ```
/// use core::fmt::Display;
/// use inline_dyn::{DynClone, dyn_star, inline_dyn};
///
/// trait DynValue: DynClone + Display {}
/// impl<T: Clone + Display> DynValue for T {}
///
/// let val: dyn_star!(DynValue) = inline_dyn!(5u8);
/// let val2 = val.clone();
/// assert_eq!(val2.to_string(), "5");
/// ```
pub unsafe trait DynClone {
    /// Writes a copy of the value into `storage`.
    ///
    /// # Safety
    ///
    /// The provided `storage` must have a size and alignment suitable
    /// for storing a clone of `self`.
    #[doc(hidden)]
    unsafe fn dyn_clone_into(&self, storage: &mut [MaybeUninit<u8>]);
}

unsafe impl<T> DynClone for T
where
    T: Clone,
{
    unsafe fn dyn_clone_into(&self, storage: &mut [MaybeUninit<u8>]) {
        assert!(mem::size_of::<Self>() <= storage.len());
        let this = (*self).clone();
        unsafe {
            storage.as_mut_ptr().cast::<T>().write(this);
        }
    }
}

unsafe impl<T> DynClone for [T]
where
    T: Clone,
{
    unsafe fn dyn_clone_into(&self, storage: &mut [MaybeUninit<u8>]) {
        struct Guard<'a, T> {
            slice: &'a mut [MaybeUninit<T>],
            init: usize,
        }

        impl<T> Drop for Guard<'_, T> {
            fn drop(&mut self) {
                // SAFETY: the guard owns the contents of the slice until it is
                // forgotten at the end of the function and `init` is updated
                // after each element is successfully cloned and written, so the
                // slice is guaranteed to be initialized and safe to drop.
                unsafe {
                    let initialized_part: &mut [T] = mem::transmute(&mut self.slice[..self.init]);
                    ptr::drop_in_place(initialized_part);
                }
            }
        }

        let len = self.len();
        // SAFETY: the precondition for this function requires the slice to have
        // a compatible layout
        let dst = unsafe {
            slice::from_raw_parts_mut(storage.as_mut_ptr().cast::<MaybeUninit<T>>(), len)
        };
        let mut guard = Guard {
            slice: dst,
            init: 0,
        };
        for (src, dst) in self.iter().zip(guard.slice.iter_mut()) {
            dst.write(src.clone());
            guard.init += 1;
        }
        mem::forget(guard);
    }
}

unsafe impl DynClone for str {
    unsafe fn dyn_clone_into(&self, storage: &mut [MaybeUninit<u8>]) {
        assert!(self.len() <= storage.len());
        // SAFETY: &str and &[MaybeUninit<u8>] have the same layout
        let this: &[MaybeUninit<u8>] = unsafe { mem::transmute(self) };
        storage[..self.len()].copy_from_slice(this);
    }
}

impl<D: DynClone + ?Sized, const S: usize, const A: usize> Clone for InlineDyn<D, S, A>
where
    Align<A>: Alignment,
{
    fn clone(&self) -> Self {
        let mut storage = RawStorage::new();
        // SAFETY: The implementation requirement for `DynClone` ensures a valid
        // value of the same type as the current value is written to `storage`,
        // so the metadata and storage layout are already known to be correct.
        unsafe {
            self.get_ref()
                .dyn_clone_into(slice::from_raw_parts_mut(storage.as_mut_ptr(), S));
            Self {
                storage,
                metadata: self.metadata,
                _marker: PhantomData,
            }
        }
    }
}

/// A convenience macro that allows for using similar syntax to the proposed
/// [`dyn* Trait`] feature.
///
/// It can be used as a type alias for an [`InlineDyn`] of the specified trait
/// with pointer sized storage.
///
/// [`dyn* Trait`]: https://github.com/rust-lang/rust/issues/102425
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
        /// Constructs a new [`InlineDyn`] containing the given value.
        ///
        /// The size and alignment of the internal storage must be large enough
        /// to store the given value, otherwise a compiler error is emitted.
        ///
        /// # Examples
        /// ```
        /// use inline_dyn::{fmt::InlineDynDisplay, inline_dyn};
        ///
        /// let val: InlineDynDisplay = inline_dyn!(42usize);
        /// assert_eq!(val.to_string(), "42");
        /// ```
        ///
        /// Trait not implemented:
        /// ```compile_fail
        /// use inline_dyn::{hash::InlineDynHasher, inline_dyn};
        /// let value: InlineDynHasher = inline_dyn!(5u8);
        /// ```
        #[macro_export]
        macro_rules! inline_dyn {
            ($e:expr) => {
                $crate::InlineDyn::new($e)
            };
        }
        #[macro_export]
        macro_rules! inline_dyn_try {
            ($e:expr) => {
                $crate::InlineDyn::try_new($e)
            };
        }

        #[cfg(feature = "alloc")]
        #[macro_export]
        macro_rules! inline_dyn_box {
            ($e:expr) => {
                $crate::InlineDyn::try_or_box($e)
            };
        }
    } else {
        /// Constructs a new [`InlineDyn`] containing the given value.
        ///
        /// The size and alignment of the internal storage must be large enough to
        /// store the given value, otherwise a compiler error is emitted.
        ///
        /// # Examples
        /// ```
        /// use inline_dyn::{fmt::InlineDynDisplay, inline_dyn};
        ///
        /// let val: InlineDynDisplay = inline_dyn!(42usize);
        /// assert_eq!(val.to_string(), "42");
        /// ```
        ///
        /// Trait not implemented:
        /// ```compile_fail
        /// use inline_dyn::{hash::InlineDynHasher, inline_dyn};
        /// let value: InlineDynHasher = inline_dyn!(5u8);
        /// ```
        #[macro_export]
        macro_rules! inline_dyn {
            ($e:expr) => {{
                let value = $e;
                // SAFETY: the lambda acts as witness that the type of the value
                // is coercible to the target type.
                unsafe {
                    $crate::InlineDyn::with_cast(value, |p| p)
                }
            }};
        }

        #[macro_export]
        macro_rules! inline_dyn_try {
            ($e:expr) => {{
                let value = $e;
                // SAFETY: the lambda acts as witness that the type of the value
                // is coercible to the target type.
                unsafe {
                    $crate::InlineDyn::try_with_cast(value, |p| p)
                }
            }};
        }

        #[cfg(feature = "alloc")]
        #[doc(hidden)]
        pub fn new_box<T>(val: T) -> std_alloc::boxed::Box<T> {
            std_alloc::boxed::Box::new(val)
        }

        #[cfg(feature = "alloc")]
        #[macro_export]
        macro_rules! inline_dyn_box {
            ($e:expr) => {{
                $crate::inline_dyn_try!($e).unwrap_or_else(|v| $crate::inline_dyn!($crate::new_box(v)))
            }};
        }

        impl<T, const S: usize, const A: usize> InlineDyn<[T], S, A>
        where
            Align<A>: Alignment,
        {
            /// Constructs a new [`InlineDyn`] containing the given value.
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
                unsafe { Self::with_cast(value, |p| p) }
            }

            pub fn try_new<const N: usize>(value: [T; N]) -> Result<Self, [T; N]> {
                unsafe { Self::try_with_cast(value, |p| p) }
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
                inline_dyn!(value)
            }

            pub fn try_new<_T: $trait + 'a>(value: _T) -> Result<Self, _T> {
                inline_dyn_try!(value)
            }

            #[cfg(feature = "alloc")]
            pub fn try_or_box<_T>(value: _T) -> Self
            where _T: $trait + 'a, std_alloc::boxed::Box<_T>: $trait + 'a, {
                Self::try_new(value).unwrap_or_else(|v| Self::new(crate::new_box(v)))
            }
        }
    };
}

impl<T, const S: usize, const A: usize, const N: usize> TryFrom<InlineDyn<[T], S, A>> for [T; N]
where
    Align<A>: Alignment,
{
    type Error = InlineDyn<[T], S, A>;

    fn try_from(value: InlineDyn<[T], S, A>) -> Result<Self, Self::Error> {
        if value.len() != N {
            return Err(value);
        }
        let value = ManuallyDrop::new(value);
        // SAFETY: The value has been wrapped in a `ManuallyDrop` and
        // the length of the slice has been checked.
        unsafe { Ok(value.as_ptr().cast::<[T; N]>().read()) }
    }
}

// impl<T, const S: usize, const A: usize> IntoIterator for InlineDyn<[T], S, A>
// where
//     Align<A>: Alignment,
// {
//     type Item = T;
//     type IntoIter = IntoIter<T, S, A>;

//     fn into_iter(self) -> Self::IntoIter {
//         IntoIter {
//             storage: ManuallyDrop::new(self),
//             pos: 0,
//         }
//     }
// }

pub struct IntoIter<T, const S: usize, const A: usize>
where
    Align<A>: Alignment,
{
    storage: ManuallyDrop<InlineDyn<[T], S, A>>,
    pos: usize,
}

impl<T, const S: usize, const A: usize> Drop for IntoIter<T, S, A>
where
    Align<A>: Alignment,
{
    fn drop(&mut self) {
        unsafe {
            ptr::drop_in_place(&mut self.storage[self.pos..]);
        }
    }
}

impl<T, const S: usize, const A: usize> IntoIter<T, S, A>
where
    Align<A>: Alignment,
{
    pub const fn new(inner: InlineDyn<[T], S, A>) -> Self {
        Self {
            storage: ManuallyDrop::new(inner),
            pos: 0,
        }
    }
}

impl<T, const S: usize, const A: usize> Iterator for IntoIter<T, S, A>
where
    Align<A>: Alignment,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.storage.len() {
            let ret = unsafe { self.storage.as_ptr().add(self.pos).read() };
            self.pos += 1;
            Some(ret)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let left = self.storage.len() - self.pos;
        (left, Some(left))
    }
}

impl<T, const S: usize, const A: usize> ExactSizeIterator for IntoIter<T, S, A> where
    Align<A>: Alignment
{
}

impl<T, const S: usize, const A: usize> FusedIterator for IntoIter<T, S, A> where Align<A>: Alignment
{}

#[cfg(all(feature = "nightly", feature = "alloc"))]
pub mod alloc {
    use core::{
        alloc::{AllocError, Allocator, Layout},
        ptr::NonNull,
    };

    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};

    pub type InlineDynAllocator<const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Allocator, S, A>;

    unsafe impl<D, const S: usize, const A: usize> Allocator for InlineDyn<D, S, A>
    where
        Align<A>: Alignment,
        D: Allocator,
    {
        fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
            self.get_ref().allocate(layout)
        }

        unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
            // SAFETY: safety of call is a precondition.
            unsafe { self.get_ref().deallocate(ptr, layout) }
        }

        fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
            self.get_ref().allocate_zeroed(layout)
        }

        unsafe fn grow(
            &self,
            ptr: NonNull<u8>,
            old_layout: Layout,
            new_layout: Layout,
        ) -> Result<NonNull<[u8]>, AllocError> {
            // SAFETY: safety of call is a precondition.
            unsafe { self.get_ref().grow(ptr, old_layout, new_layout) }
        }

        unsafe fn grow_zeroed(
            &self,
            ptr: NonNull<u8>,
            old_layout: Layout,
            new_layout: Layout,
        ) -> Result<NonNull<[u8]>, AllocError> {
            // SAFETY: safety of call is a precondition.
            unsafe { self.get_ref().grow_zeroed(ptr, old_layout, new_layout) }
        }

        unsafe fn shrink(
            &self,
            ptr: NonNull<u8>,
            old_layout: Layout,
            new_layout: Layout,
        ) -> Result<NonNull<[u8]>, AllocError> {
            // SAFETY: safety of call is a precondition.
            unsafe { self.get_ref().shrink(ptr, old_layout, new_layout) }
        }
    }
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
            inline_dyn!(value)
        }
    }

    impl<D, const S: usize, const A: usize> InlineDyn<D, S, A>
    where
        Align<A>: Alignment,
        D: Any + ?Sized,
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

#[cfg(feature = "nightly")]
pub mod async_iter {
    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};
    use core::async_iter::AsyncIterator;

    pub type InlineDynAsyncIterator<'a, T, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn AsyncIterator<Item = T> + 'a, S, A>;

    impl_new!(AsyncIterator<Item = T>, T);

    impl<I, const S: usize, const A: usize> AsyncIterator for InlineDyn<I, S, A>
    where
        Align<A>: Alignment,
        I: AsyncIterator + ?Sized,
    {
        type Item = I::Item;

        fn poll_next(
            self: core::pin::Pin<&mut Self>,
            cx: &mut core::task::Context<'_>,
        ) -> core::task::Poll<Option<Self::Item>> {
            self.get_pinned_mut().poll_next(cx)
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            self.get_ref().size_hint()
        }
    }
}

pub mod borrow {

    use crate::{InlineDyn, DEFAULT_SIZE};

    pub type InlineDynBorrow<'a, T, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn core::borrow::Borrow<T> + 'a, S, A>;
    pub type InlineDynBorrowMut<'a, T, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn core::borrow::BorrowMut<T> + 'a, S, A>;

    impl_new!(core::borrow::Borrow<T>, T);
    impl_new!(core::borrow::BorrowMut<T>, T);
}

pub mod convert {
    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};

    pub type InlineDynAsRef<'a, U, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn core::convert::AsRef<U> + 'a, S, A>;
    pub type InlineDynAsMut<'a, U, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn core::convert::AsMut<U> + 'a, S, A>;

    impl_new!(AsRef<U>, U);
    impl_new!(AsMut<U>, U);

    impl<T, U, const S: usize, const A: usize> AsRef<U> for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: AsRef<U> + ?Sized,
        U: ?Sized,
    {
        fn as_ref(&self) -> &U {
            self.get_ref().as_ref()
        }
    }

    impl<T, U, const S: usize, const A: usize> AsMut<U> for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: AsMut<U> + ?Sized,
        U: ?Sized,
    {
        fn as_mut(&mut self) -> &mut U {
            self.get_mut().as_mut()
        }
    }
}

pub mod fmt {
    use core::fmt::{
        Binary, Debug, Display, Formatter, LowerExp, LowerHex, Octal, Pointer, Result as FmtResult,
        UpperExp, UpperHex, Write,
    };

    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};

    pub type InlineDynBinary<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Binary + 'a, S, A>;
    pub type InlineDynDebug<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Debug + 'a, S, A>;
    pub type InlineDynDisplay<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Display + 'a, S, A>;
    pub type InlineDynLowerExp<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn LowerExp + 'a, S, A>;
    pub type InlineDynLowerHex<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn LowerHex + 'a, S, A>;
    pub type InlineDynOctal<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Octal + 'a, S, A>;
    pub type InlineDynPointer<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Pointer + 'a, S, A>;
    pub type InlineDynUpperExp<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn UpperExp + 'a, S, A>;
    pub type InlineDynUpperHex<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn UpperHex + 'a, S, A>;
    pub type InlineDynWrite<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Write + 'a, S, A>;

    impl_new!(Binary);
    impl_new!(Debug);
    impl_new!(Display);
    impl_new!(LowerExp);
    impl_new!(LowerHex);
    impl_new!(Octal);
    impl_new!(Pointer);
    impl_new!(UpperExp);
    impl_new!(UpperHex);
    impl_new!(Write);

    impl<T, const S: usize, const A: usize> Binary for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: Binary + ?Sized,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            Binary::fmt(self.get_ref(), f)
        }
    }

    impl<T, const S: usize, const A: usize> Debug for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: Debug + ?Sized,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            Debug::fmt(self.get_ref(), f)
        }
    }

    impl<T, const S: usize, const A: usize> Display for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: Display + ?Sized,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            Display::fmt(self.get_ref(), f)
        }
    }

    impl<T, const S: usize, const A: usize> LowerExp for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: LowerExp + ?Sized,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            LowerExp::fmt(self.get_ref(), f)
        }
    }

    impl<T, const S: usize, const A: usize> LowerHex for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: LowerHex + ?Sized,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            LowerHex::fmt(self.get_ref(), f)
        }
    }

    impl<T, const S: usize, const A: usize> Octal for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: Octal + ?Sized,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            Octal::fmt(self.get_ref(), f)
        }
    }

    impl<T, const S: usize, const A: usize> Pointer for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: Pointer + ?Sized,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            Pointer::fmt(self.get_ref(), f)
        }
    }

    impl<T, const S: usize, const A: usize> UpperExp for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: UpperExp + ?Sized,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            UpperExp::fmt(self.get_ref(), f)
        }
    }

    impl<T, const S: usize, const A: usize> UpperHex for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: UpperHex + ?Sized,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            UpperHex::fmt(self.get_ref(), f)
        }
    }

    impl<T, const S: usize, const A: usize> Write for InlineDyn<T, S, A>
    where
        Align<A>: Alignment,
        T: Write + ?Sized,
    {
        fn write_str(&mut self, s: &str) -> FmtResult {
            self.get_mut().write_str(s)
        }

        fn write_char(&mut self, c: char) -> FmtResult {
            self.get_mut().write_char(c)
        }

        fn write_fmt(&mut self, args: core::fmt::Arguments<'_>) -> FmtResult {
            self.get_mut().write_fmt(args)
        }
    }
}

pub mod future {
    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};
    use core::{
        future::Future,
        pin::Pin,
        task::{Context, Poll},
    };

    pub type InlineDynFuture<'a, T, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Future<Output = T> + 'a, S, A>;

    impl_new!(Future<Output = T>, T);

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

    impl_new!(Hasher);

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
    pub type InlineDynIntoIterator<'a, I, T, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn IntoIterator<IntoIter = I, Item = T> + 'a, S, A>;

    impl_new!(Iterator<Item = T>, T);
    impl_new!(DoubleEndedIterator<Item = T>, T);
    impl_new!(IntoIterator<IntoIter = I, Item = T>, I, T);

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

        #[cfg(feature = "nightly")]
        fn advance_by(&mut self, n: usize) -> Result<(), core::num::NonZeroUsize> {
            self.get_mut().advance_by(n)
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

        #[cfg(feature = "nightly")]
        fn advance_back_by(&mut self, n: usize) -> Result<(), core::num::NonZeroUsize> {
            self.get_mut().advance_back_by(n)
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

#[cfg(any(feature = "std", feature = "nightly"))]
pub mod error {
    #[cfg(feature = "nightly")]
    use core::error::Error;
    #[cfg(not(feature = "nightly"))]
    use std::error::Error;

    use crate::{Align, Alignment, InlineDyn, DEFAULT_SIZE};

    pub type InlineDynError<'a, const S: usize = DEFAULT_SIZE, const A: usize = S> =
        InlineDyn<dyn Error + 'a, S, A>;

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

        #[cfg(feature = "nightly")]
        fn provide<'a>(&'a self, request: &mut core::error::Request<'a>) {
            self.get_ref().provide(request)
        }
    }
}

cfg_if! {
    if #[cfg(feature = "std")] {
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

                #[cfg(feature = "nightly")]
                fn is_read_vectored(&self) -> bool {
                    self.get_ref().is_read_vectored()
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

                #[cfg(feature = "nightly")]
                fn is_write_vectored(&self) -> bool {
                    self.get_ref().is_write_vectored()
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
    use core::{cell::Cell, panic::AssertUnwindSafe};
    use std::panic;

    use super::{fmt::InlineDynDebug, *};

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
    fn test_simple() {
        let val = <dyn_star!(Debug)>::new(42usize);
        assert_eq!(format!("{val:?}"), "42");
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
            assert_eq!(format!("{dbg:?}"), "Dropper(false)");
        }
        assert!(dropped);
    }

    #[test]
    fn test_resize() {
        let val = <dyn_star!(Debug)>::new(42u8);
        assert_eq!(format!("{val:?}"), "42");

        let val: InlineDynDebug<1> = <dyn_star!(Debug)>::try_resize(val).ok().unwrap();
        assert_eq!(format!("{val:?}"), "42");
    }

    #[test]
    fn test_resize_insufficient() {
        let val = <dyn_star!(Debug)>::new(42u32);
        assert_eq!(format!("{val:?}"), "42");

        let res: Result<InlineDynDebug<1>, _> = <dyn_star!(Debug)>::try_resize(val);
        assert_matches!(res, Err(_));
    }

    #[test]
    fn test_slice() {
        let val = InlineDyn::<[u8], 4>::new([1, 2, 3, 4]);
        assert_eq!(val.get_ref(), [1, 2, 3, 4]);

        let res = InlineDyn::<[u8], 3, 1>::try_new([1, 2, 3, 4]);
        assert_matches!(res, Err(_));
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

        let val: dyn_star!(Foo) = inline_dyn!(Bar(Cell::new(0)));
        assert_eq!(val.foo(), 0);
        assert_eq!(val.foo(), 1);
        assert_eq!(val.foo(), 2);
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
                    0 => Poll::Ready(()),
                    _ => {
                        *amt -= 1;
                        Poll::Pending
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

    #[test]
    fn test_clone() {
        trait DynValue: DynClone + Debug {}
        impl<T: Clone + Debug> DynValue for T {}

        #[derive(Debug)]
        struct Dropper<'a>(&'a Cell<(usize, usize)>);

        impl Clone for Dropper<'_> {
            fn clone(&self) -> Self {
                let (cloned, dropped) = self.0.get();
                self.0.set((cloned + 1, dropped));
                Self(self.0)
            }
        }

        impl Drop for Dropper<'_> {
            fn drop(&mut self) {
                let (cloned, dropped) = self.0.get();
                self.0.set((cloned, dropped + 1));
            }
        }

        let dropped = Cell::new((0, 0));
        let val: dyn_star!(DynValue, '_) = inline_dyn!(Dropper(&dropped));
        assert_eq!(dropped.get(), (0, 0));
        let val2 = val.clone();
        assert_eq!(dropped.get(), (1, 0));
        drop(val);
        assert_eq!(dropped.get(), (1, 1));
        drop(val2);
        assert_eq!(dropped.get(), (1, 2));
    }

    #[test]
    fn test_clone_slice() {
        #[derive(Debug, Clone)]
        struct Dropper<'a>(&'a Cell<usize>);

        impl Drop for Dropper<'_> {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
            }
        }

        let dropped = Cell::new(0usize);
        let value = <InlineDyn<[_], 1024, 16>>::new([
            Dropper(&dropped),
            Dropper(&dropped),
            Dropper(&dropped),
            Dropper(&dropped),
        ]);
        let cloned = value.clone();
        assert_eq!(dropped.get(), 0);
        drop(value);
        assert_eq!(dropped.get(), 4);
        drop(cloned);
        assert_eq!(dropped.get(), 8);
    }

    #[test]
    fn test_clone_panic() {
        #[derive(Debug)]
        struct Dropper<'a>(&'a Cell<(usize, usize)>);

        impl Clone for Dropper<'_> {
            fn clone(&self) -> Self {
                let (cloned, dropped) = self.0.get();
                if cloned == 3 {
                    panic!("boom!");
                }
                self.0.set((cloned + 1, dropped));
                Self(self.0)
            }
        }

        impl Drop for Dropper<'_> {
            fn drop(&mut self) {
                let (cloned, dropped) = self.0.get();
                self.0.set((cloned, dropped + 1));
            }
        }

        let dropped = Cell::new((0usize, 0usize));
        let value = <InlineDyn<[_], 1024, 16>>::new([
            Dropper(&dropped),
            Dropper(&dropped),
            Dropper(&dropped),
            Dropper(&dropped),
        ]);
        panic::catch_unwind(AssertUnwindSafe(|| {
            assert_eq!(dropped.get(), (0, 0));
            value.clone()
        }))
        .expect_err("clone should have panicked");
        assert_eq!(dropped.get(), (3, 3));
        drop(value);
        assert_eq!(dropped.get(), (3, 7));
    }
}
