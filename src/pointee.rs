use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "nightly")] {
        use core::{ops::CoerceUnsized, ptr::Pointee};
        pub(crate) use core::ptr::{from_raw_parts, from_raw_parts_mut};

        pub(crate) type Metadata<T> = <T as Pointee>::Metadata;

        pub(crate) const fn unsize<D, T>(ptr: *const T) -> Metadata<D>
        where D: ?Sized, *const T: CoerceUnsized<*const D> {
            core::ptr::metadata::<D>(ptr)
        }
    } else {
        pub(crate) struct Metadata<T: ?Sized>(fn(*const ()) -> *const T);

        impl<T: ?Sized> Copy for Metadata<T> {}
        impl<T: ?Sized> Clone for Metadata<T> {
            fn clone(&self) -> Self { *self }
        }

        impl<T: ?Sized> Metadata<T> {
            pub(crate) const fn new(cast: fn(*const ()) -> *const T) -> Self {
                Self(cast)
            }
        }

        pub(crate) unsafe fn from_raw_parts<T>(addr: *const (), metadata: Metadata<T>) -> *const T
        where T: ?Sized {
            (metadata.0)(addr)
        }

        pub(crate) unsafe fn from_raw_parts_mut<T>(addr: *mut (), metadata: Metadata<T>) -> *mut T
        where T: ?Sized {
            (metadata.0)(addr).cast_mut()
        }
    }
}
