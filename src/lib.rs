#![cfg_attr(feature = "nightly", feature(unsize, const_generics))]
#![cfg_attr(not(any(feature = "std", test)), no_std)]
use core::{
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    marker::PhantomData,
    mem::{self, MaybeUninit},
    ops::{Deref, DerefMut},
};

pub type DefaultStorage = [usize; 1];

#[cfg(feature = "nightly")]
mod nightly;

// TODO: Replace with DynMetadata if RFC 2580 is accepted.
#[doc(hidden)]
pub struct VTable<T: ?Sized> {
    pub cast_ref: unsafe fn(*const ()) -> *const T,
    pub cast_mut: unsafe fn(*mut ()) -> *mut T,
}

pub struct InlineDyn<'a, D: ?Sized + 'a, T = DefaultStorage> {
    vtable: &'a VTable<D>,
    // TODO: Switch to using RawStorage once const generics are stabilized.
    storage: MaybeUninit<T>,
    _marker: PhantomData<D>,
}

impl<'a, D: ?Sized, T> InlineDyn<'a, D, T> {
    #[doc(hidden)]
    pub unsafe fn with_vtable<U: Sized + 'a>(vtable: &'a VTable<D>, value: U) -> Result<Self, U> {
        if (mem::size_of_val(&value) <= mem::size_of::<T>())
            && (mem::align_of_val(&value) <= mem::align_of::<T>())
        {
            let mut storage = MaybeUninit::<T>::uninit();
            storage.as_mut_ptr().cast::<U>().write(value);
            Ok(Self {
                vtable,
                storage,
                _marker: PhantomData,
            })
        } else {
            Err(value)
        }
    }

    pub fn get_ref(&self) -> &D {
        unsafe { &*(self.vtable.cast_ref)(self.storage.as_ptr().cast()) }
    }

    pub fn get_mut(&mut self) -> &mut D {
        unsafe { &mut *(self.vtable.cast_mut)(self.storage.as_mut_ptr().cast()) }
    }

    pub fn try_resize<U>(this: Self) -> Result<InlineDyn<'a, D, U>, Self> {
        let (size, align) = {
            let val = this.get_ref();
            (mem::size_of_val(val), mem::align_of_val(val))
        };
        if (size <= mem::size_of::<U>()) && (align <= mem::align_of::<U>()) {
            unimplemented!()
        } else {
            Err(this)
        }
    }
}

impl<D: ?Sized, T> Drop for InlineDyn<'_, D, T> {
    fn drop(&mut self) {
        unsafe {
            (self.get_mut() as *mut D).drop_in_place();
        }
    }
}

impl<D: ?Sized, T> Deref for InlineDyn<'_, D, T> {
    type Target = D;

    fn deref(&self) -> &Self::Target {
        self.get_ref()
    }
}

impl<D: ?Sized, T> DerefMut for InlineDyn<'_, D, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

impl<D: Debug + ?Sized, T> Debug for InlineDyn<'_, D, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(self.get_ref(), f)
    }
}

impl<D: Display + ?Sized, T> Display for InlineDyn<'_, D, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(self.get_ref(), f)
    }
}

#[macro_export]
#[cfg(not(feature = "nightly"))]
macro_rules! inline_dyn {
    ($trait:path; $e:expr) => {{
        fn assert_impl<'a, T: $trait + 'a>(t: T) -> (&'a $crate::VTable<dyn $trait + 'a>, T) {
            (
                &$crate::VTable {
                    cast_ref: |p| p.cast::<T>(),
                    cast_mut: |p| p.cast::<T>(),
                },
                t,
            )
        }
        unsafe {
            let (vtable, t) = assert_impl($e);
            $crate::InlineDyn::<'_, dyn $trait, _>::with_vtable(vtable, t)
        }
    }};
}

#[macro_export]
#[cfg(feature = "nightly")]
macro_rules! inline_dyn {
    ($trait:path; $e:expr) => {{
        $crate::InlineDyn::<'_, dyn $trait, _>::try_new($e)
    }};
}

pub mod any {
    use core::{
        any::{Any, TypeId},
        mem::ManuallyDrop,
    };

    pub type InlineDynAny<T = crate::DefaultStorage> = crate::InlineDyn<'static, dyn Any, T>;

    impl<T> InlineDynAny<T> {
        pub fn try_from<U: Any>(value: U) -> Result<Self, U> {
            inline_dyn![Any; value]
        }
    }

    impl<D: Any + ?Sized, T> crate::InlineDyn<'static, D, T> {
        pub fn downcast<U: Any>(self) -> Result<U, Self> {
            if self.get_ref().type_id() == TypeId::of::<U>() {
                let this = ManuallyDrop::new(self);
                unsafe { Ok((this.get_ref() as *const D).cast::<U>().read()) }
            } else {
                Err(self)
            }
        }
    }
}

pub mod convert {
    pub type InlineDynAsRef<'a, U, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn core::convert::AsRef<U>, T>;
    pub type InlineDynAsMut<'a, U, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn core::convert::AsMut<U>, T>;
}

pub mod fmt {
    pub type InlineDynDebug<'a, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn core::fmt::Debug, T>;
    pub type InlineDynDisplay<'a, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn core::fmt::Display, T>;

    impl<'a, T> InlineDynDebug<'a, T> {
        pub fn try_from<U: core::fmt::Debug + 'a>(value: U) -> Result<Self, U> {
            inline_dyn![core::fmt::Debug; value]
        }
    }

    impl<'a, T> InlineDynDisplay<'a, T> {
        pub fn try_from<U: core::fmt::Display + 'a>(value: U) -> Result<Self, U> {
            inline_dyn![core::fmt::Display; value]
        }
    }
}

pub mod hash {
    pub type InlineDynHasher<'a, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn core::hash::Hasher, T>;
}

#[cfg(any(feature = "std", test))]
pub mod io {
    pub type InlineDynRead<'a, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn std::io::Read, T>;
    pub type InlineDynWrite<'a, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn std::io::Write, T>;
    pub type InlineDynSeek<'a, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn std::io::Seek, T>;
    pub type InlineDynBufRead<'a, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn std::io::BufRead, T>;
}

pub mod iter {
    pub type InlineDynIterator<'a, I, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn Iterator<Item = I>, T>;
    pub type InlineDynDoubleEndedIterator<'a, I, T = crate::DefaultStorage> =
        crate::InlineDyn<'a, dyn DoubleEndedIterator<Item = I>, T>;
}

pub mod ops {
    #[macro_export]
    macro_rules! InlineDynFn {
        (($($Args:ty),*) $(-> $R:ty)?) => {
            $crate::InlineDynFn!(($($Args),*) $(-> $R)?; $crate::DefaultStorage)
        };
        (($($Args:ty),*) $(-> $R:ty)?; $S:ty) => {
            $crate::InlineDyn<dyn Fn($($Args),*)$(-> $R)?, $S>
        };
    }

    #[macro_export]
    macro_rules! InlineDynFnMut {
        (($($Args:ty),*) $(-> $R:ty)?) => {
            $crate::InlineDynFnMut!(($($Args),*) $(-> $R)?; $crate::DefaultStorage)
        };
        (($($Args:ty),*) $(-> $R:ty)?; $S:ty) => {
            $crate::InlineDyn<'_, dyn FnMut($($Args),*)$(-> $R)?, $S>
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmt::{InlineDynDebug, InlineDynDisplay};

    #[test]
    fn test_simple() {
        let val: InlineDynDisplay = InlineDynDisplay::try_from(42).unwrap();
        assert_eq!(val.to_string(), "42");
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
    fn test_size() {
        let res: Result<InlineDynDisplay<u8>, _> = InlineDynDisplay::try_from(42usize);
        assert!(res.is_err());
    }

    #[test]
    fn test_alignment() {
        let res: Result<InlineDynDisplay<[u8; 8]>, _> = InlineDynDisplay::try_from(42usize);
        assert!(res.is_err());
    }
}
