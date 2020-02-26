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

use self::storage::RawStorage;

#[cfg(feature = "nightly")]
mod nightly;
mod storage;

pub use storage::{Align, DefaultSize, Size};

struct VTable<D: ?Sized> {
    cast_ref: unsafe fn(*const ()) -> *const D,
    cast_mut: unsafe fn(*mut ()) -> *mut D,
}

impl<D: ?Sized> VTable<D> {
    unsafe fn with_unsize<'a, C, T: 'a>() -> &'a Self
    where C: Coerce<T, D> {
        &Self {
            cast_ref: |p| C::coerce(p.cast::<T>()),
            cast_mut: |p| C::coerce(p.cast::<T>() as *mut _) as *mut _,
        }
    }
}

#[doc(hidden)]
pub unsafe trait Coerce<T, U: ?Sized> {
    fn coerce(p: *const T) -> *const U;
}

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
    #[doc(hidden)]
    pub unsafe fn with_unsize<C, T: 'a>(value: T) -> Result<Self, T>
    where C: Coerce<T, D> {
        Self::with_metadata(VTable::with_unsize::<C, T>(), value)
    }

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

    pub fn get_ref(&self) -> &D {
        unsafe { &*(self.metadata.cast_ref)(self.storage.as_ptr().cast()) }
    }

    pub fn get_mut(&mut self) -> &mut D {
        unsafe { &mut *(self.metadata.cast_mut)(self.storage.as_mut_ptr().cast()) }
    }

    pub fn grow<U, V>(this: Self) -> InlineDyn<'a, D, U, V>
    where
        U: Size,
        V: Align,
        U: typenum::IsGreaterOrEqual<S>,
        V: typenum::IsGreaterOrEqual<A>, {
        let (size, align) = {
            let val = this.get_ref();
            (mem::size_of_val(val), mem::align_of_val(val))
        };
        debug_assert!(size <= mem::size_of::<RawStorage<U, V>>());
        debug_assert!(align <= mem::align_of::<RawStorage<U, V>>());

        let this = ManuallyDrop::new(this);
        let mut storage = RawStorage::<U, V>::new();
        unsafe {
            ptr::copy_nonoverlapping(this.storage.as_ptr(), storage.as_mut_ptr(), size);
        }
        InlineDyn {
            metadata: this.metadata,
            storage,
            _marker: PhantomData,
        }
    }

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
    //         let value = ManuallyDrop::new(value);
    //         let mut storage = RawStorage::<U, V>::new();
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

#[macro_export]
#[cfg(not(feature = "nightly"))]
macro_rules! inline_dyn {
    ($trait:path; $e:expr) => {{
        fn try_build<'a, T, S, A>(value: T) -> Result<$crate::InlineDyn<'a, dyn $trait + 'a, S, A>, T>
        where T: $trait + 'a, S: $crate::Size, A: $crate::Align, {
            struct Unsize;
            unsafe impl<'a, T: $trait + 'a> $crate::Coerce<T, (dyn $trait + 'a)> for Unsize {
                fn coerce(p: *const T) -> *const (dyn $trait + 'a) {
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
#[macro_export]
#[cfg(all(feature = "alloc", not(feature = "nightly")))]
macro_rules! inline_dyn_box {
    ($trait:path; $e:expr) => {{
        fn build<'a, T, S, A>(value: T) -> $crate::InlineDyn<'a, dyn $trait + 'a, S, A>
        where T: $trait + 'a, Box<T>: $trait + 'a,
            S: $crate::Size + $crate::typenum::IsGreaterOrEqual<$crate::DefaultSize>,
            A: $crate::Align + $crate::typenum::IsGreaterOrEqual<$crate::DefaultSize>, {
            struct Unsize;
            unsafe impl<'a, T: $trait + 'a> $crate::Coerce<T, (dyn $trait + 'a)> for Unsize {
                fn coerce(p: *const T) -> *const (dyn $trait + 'a) {
                    p
                }
            }

            unsafe {
                $crate::InlineDyn::with_unsize::<Unsize, _>(value)
                    .or_else(|v| $crate::InlineDyn::with_unsize(Box::new(v)))
                    .ok()
                    .expect("insufficient space for box")
            }
        }
        build($e)
    }};
}

#[macro_export]
#[cfg(feature = "nightly")]
macro_rules! inline_dyn {
    ($trait:path; $e:expr) => {{
        $crate::InlineDyn::<'_, dyn $trait, _, _>::try_new($e)
    }};
}
#[macro_export]
#[cfg(all(feature = "alloc", feature = "nightly"))]
macro_rules! inline_dyn_box {
    ($trait:path; $e:expr) => {{
        $crate::InlineDyn::<'_, dyn $trait, _, _>::try_or_box($e)
    }};
}

pub mod any {
    use core::{
        any::{Any, TypeId},
        mem::ManuallyDrop,
    };

    use crate::{Align, DefaultSize, Size};

    pub type InlineDynAny<S = DefaultSize, A = S> = crate::InlineDyn<'static, dyn Any, S, A>;

    impl<S: Size, A: Align> InlineDynAny<S, A> {
        pub fn try_from<T: Any>(value: T) -> Result<Self, T> {
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
}

pub mod fmt {
    use crate::{Align, DefaultSize, Size};

    pub type InlineDynDebug<'a, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn core::fmt::Debug, S, A>;
    pub type InlineDynDisplay<'a, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn core::fmt::Display, S, A>;

    impl<'a, S, A> InlineDynDebug<'a, S, A>
    where
        S: Size,
        A: Align,
    {
        pub fn try_from<T: core::fmt::Debug + 'a>(value: T) -> Result<Self, T> {
            inline_dyn![core::fmt::Debug; value]
        }
    }

    impl<'a, S, A> InlineDynDisplay<'a, S, A>
    where
        S: Size,
        A: Align,
    {
        pub fn try_from<T: core::fmt::Display + 'a>(value: T) -> Result<Self, T> {
            inline_dyn![core::fmt::Display; value]
        }
    }
}

pub mod hash {
    pub type InlineDynHasher<'a, S = crate::DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn core::hash::Hasher, S, A>;
}

#[cfg(any(feature = "std", test))]
pub mod io {
    use crate::DefaultSize;

    pub type InlineDynRead<'a, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn std::io::Read, S, A>;
    pub type InlineDynWrite<'a, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn std::io::Write, S, A>;
    pub type InlineDynSeek<'a, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn std::io::Seek, S, A>;
    pub type InlineDynBufRead<'a, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn std::io::BufRead, S, A>;
}

pub mod iter {
    use crate::DefaultSize;

    pub type InlineDynIterator<'a, I, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn Iterator<Item = I>, S, A>;
    pub type InlineDynDoubleEndedIterator<'a, I, S = DefaultSize, A = S> =
        crate::InlineDyn<'a, dyn DoubleEndedIterator<Item = I>, S, A>;
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

#[cfg(test)]
mod tests {
    use typenum::{U0, U1, U4};

    use super::*;
    use crate::fmt::{InlineDynDebug, InlineDynDisplay};

    #[test]
    fn test_simple() {
        let val: InlineDynDebug = InlineDynDebug::try_from(42usize).unwrap();
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
            let dbg: InlineDynDebug = InlineDynDebug::try_from(Dropper(&mut dropped)).unwrap();
            assert_eq!(format!("{:?}", dbg), "Dropper(false)");
        }
        assert_eq!(dropped, true);
    }

    #[test]
    fn test_size_insufficient() {
        let res: Result<InlineDynDebug<U0>, _> = InlineDynDebug::try_from(42u8);
        assert!(res.is_err());
    }

    #[test]
    fn test_alignment_insufficient() {
        let res: Result<InlineDynDebug<U4, U1>, _> = InlineDynDebug::try_from(42u32);
        assert!(res.is_err());
    }

    #[test]
    fn test_resize() {
        let val: InlineDynDisplay = InlineDynDisplay::try_from(42u8).unwrap();
        assert_eq!(val.to_string(), "42");

        let val: InlineDynDisplay<U1> = InlineDynDisplay::try_resize(val).ok().unwrap();
        assert_eq!(val.to_string(), "42");
    }

    #[test]
    fn test_resize_insufficient() {
        let val: InlineDynDisplay = InlineDynDisplay::try_from(42u32).unwrap();
        assert_eq!(val.to_string(), "42");

        let res: Result<InlineDynDisplay<U1>, _> = InlineDynDisplay::try_resize(val);
        assert!(res.is_err());
    }
}
