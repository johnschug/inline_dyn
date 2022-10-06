use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "nightly")] {
        use core::{ops::CoerceUnsized, ptr::Pointee};
        pub use core::ptr::{from_raw_parts, from_raw_parts_mut};

        pub type Metadata<T> = <T as Pointee>::Metadata;

        pub fn unsize<D, T>(ptr: *const T) -> Metadata<D>
        where D: ?Sized, *const T: CoerceUnsized<*const D> {
            core::ptr::metadata::<D>(ptr)
        }
    } else {
        use core::ptr::NonNull;

        #[doc(hidden)]
        pub unsafe trait Coerce<T, U: ?Sized> {
            fn coerce(p: *const T) -> *const U;
        }

        struct Vtable<T: ?Sized> {
            cast_ref: unsafe fn(*const ()) -> *const T,
            cast_mut: unsafe fn(*mut ()) -> *mut T,
        }

        impl<T: ?Sized> Vtable<T> {
            const fn new<'a, C, U: 'a>() -> &'a Self
            where C: Coerce<U, T> {
                &Vtable {
                    cast_ref: |p| C::coerce(p.cast::<U>()),
                    cast_mut: |p| C::coerce(p.cast::<U>()) as *mut _,
                }
            }
        }

        pub struct Metadata<T: ?Sized>(NonNull<Vtable<T>>);

        unsafe impl<T: ?Sized> Send for Metadata<T> {}
        unsafe impl<T: ?Sized> Sync for Metadata<T> {}

        impl<T: ?Sized> Copy for Metadata<T> {}
        impl<T: ?Sized> Clone for Metadata<T> {
            fn clone(&self) -> Self { *self }
        }

        pub fn unsize<C, D, T>() -> Metadata<D>
        where D: ?Sized, C: Coerce<T, D> {
            Metadata(NonNull::from(Vtable::new::<C, T>()))
        }

        pub unsafe fn from_raw_parts<T>(addr: *const (), metadata: Metadata<T>) -> *const T
        where T: ?Sized {
            (metadata.0.as_ref().cast_ref)(addr)
        }

        pub unsafe fn from_raw_parts_mut<T>(addr: *mut (), metadata: Metadata<T>) -> *mut T
        where T: ?Sized {
            (metadata.0.as_ref().cast_mut)(addr)
        }
    }
}
