# Picket

A lightweight, `serde`-compatible generational arena allocator for Rust.

[![Crates.io](https://img.shields.io/crates/v/picket.svg)](https://crates.io/crates/picket)
[![Documentation](https://docs.rs/picket/badge.svg)](https://docs.rs/picket)

Picket provides a `Vec`-like data structure (`Arena`) where items are accessed via a stable, generational `Index` rather than a raw integer offset. This solves the [ABA problem](https://en.wikipedia.org/wiki/ABA_problem): if you remove an item and insert a new one in its place, old indices referencing the removed item will correctly fail to retrieve the new one.

## Features

*   **Lightweight**: `Index` is 8 bytes. `Option<Index>` is also 8 bytes using `NonZeroU32`.
*   **`serde` Support**: The `Arena` can be serialized and deserialized through the `serde` feature.
*   **no_std**: Supports `no_std` environments. (`std` is enabled by default)

## Usage

```rust
use picket::Arena;

fn main() {
    let mut arena = Arena::new();

    let texture_a = arena.insert("Texture A");
    let texture_b = arena.insert("Texture B");

    assert_eq!(arena[texture_a], "Texture A");

    arena.remove(texture_a);

    assert!(arena.get(texture_a).is_none());

    let texture_c = arena.insert("Texture C");

    assert!(arena.get(texture_a).is_none());
    assert_eq!(arena[texture_c], "Texture C");
}