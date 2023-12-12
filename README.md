# inline_dyn

[![crates.io version][version]][crate]
[![docs.rs][docs badge]][docs]
[![Build Status][build]][ci]

A container type for storing dynamically-sized types (e.g., trait objects, slices) inline.

# Example

Creating an owned trait object without dynamic allocation:
```rust
use inline_dyn::fmt::InlineDynDisplay;

// Can store any type that implements `Display` as long as it is layout
// compatible with `usize`.
let val: InlineDynDisplay = <InlineDynDisplay>::new(42u8);
assert_eq!(val.to_string(), "42");
```

# License
Licensed under either of Apache License (Version 2.0) or MIT license at your
discretion.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.

[version]: https://img.shields.io/crates/v/inline_dyn?style=flat-square
[crate]: https://crates.io/crates/inline_dyn
[docs badge]: https://img.shields.io/badge/docs.rs-inline__dyn-yellow?style=flat-square
[docs]: https://docs.rs/inline_dyn
[build]: https://img.shields.io/github/actions/workflow/status/johnschug/inline_dyn/ci.yml?branch=master&style=flat-square
[ci]: https://github.com/johnschug/inline_dyn/actions/workflows/ci.yml
