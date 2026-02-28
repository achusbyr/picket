//! # Picket
//!
//! Picket is a lightweight, `serde`-compatible generational arena allocator.
//!
//! It allows you to insert items into an arena and receive a unique `Index` in return.
//! Unlike a standard `Vec` index, a Picket `Index` contains a "generation" counter.
//! If an item is removed and its slot is reused by a new item, the old `Index` will
//! fail to access the new item, preventing [ABA problems](https://en.wikipedia.org/wiki/ABA_problem).
//!
//! ## Key Features
//!
//! *   **Compact**: `Index` contains no padding.
//! *   **Serde**: Optional support for full serialization/deserialization (feature: `serde`).
//! *   **no_std**: Compatible with `no_std`.

#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

macro_rules! define_arena {
    (
        $idx_type:ty,
        $nonzero_type:ident
    ) => {
        use alloc::vec::Vec;
        use core::fmt;
        use core::num::$nonzero_type;
        use core::ops::{Index as OpsIndex, IndexMut as OpsIndexMut};

        /// A unique key to access values in an [`Arena`].
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct Index {
            slot: $idx_type,
            generation: $nonzero_type,
        }

        impl Index {
            /// Returns the raw slot index (index into the backing vector).
            #[inline]
            pub fn slot(&self) -> $idx_type {
                self.slot
            }

            /// Returns the raw generation of this index.
            #[inline]
            pub fn generation(&self) -> $idx_type {
                self.generation.get()
            }

            /// Creates an Index from raw parts.
            /// Returns None if generation is 0.
            pub fn from_raw_parts(slot: $idx_type, generation: $idx_type) -> Option<Self> {
                <$nonzero_type>::new(generation).map(|generation| Self { slot, generation })
            }
        }

        impl fmt::Debug for Index {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "Index({}v{})", self.slot, self.generation)
            }
        }

        /// Internal storage entry.
        #[derive(Clone, Debug)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        enum Entry<T> {
            /// A free slot. The `generation` is the generation the *next* occupant will get.
            Free {
                next_free: Option<$idx_type>,
                generation: $nonzero_type,
            },
            /// An occupied slot. The `generation` is the current generation of the value.
            Occupied { generation: $nonzero_type, value: T },
        }

        impl<T> Entry<T> {
            // Used in test
            #[allow(dead_code)]
            fn generation(&self) -> $nonzero_type {
                match self {
                    Entry::Free { generation, .. } => *generation,
                    Entry::Occupied { generation, .. } => *generation,
                }
            }
        }

        /// A lightweight, generational arena allocator.
        #[derive(Clone, Debug)]
        pub struct Arena<T> {
            items: Vec<Entry<T>>,
            /// The index of the first free slot in the linked list.
            free_head: Option<$idx_type>,
            /// Number of active items.
            len: usize,
        }

        impl<T> Default for Arena<T> {
            fn default() -> Self {
                Self::new()
            }
        }

        impl<T> Arena<T> {
            /// Creates a new empty arena.
            pub fn new() -> Self {
                Self {
                    items: Vec::new(),
                    free_head: None,
                    len: 0,
                }
            }

            /// Creates a new arena with space for at least `capacity` items.
            pub fn with_capacity(capacity: usize) -> Self {
                Self {
                    items: Vec::with_capacity(capacity),
                    free_head: None,
                    len: 0,
                }
            }

            /// Returns the number of elements currently in the arena.
            pub fn len(&self) -> usize {
                self.len
            }

            /// Returns true if the arena contains no elements.
            pub fn is_empty(&self) -> bool {
                self.len == 0
            }

            /// Returns the total capacity of the arena.
            pub fn capacity(&self) -> usize {
                self.items.capacity()
            }

            /// Reserves capacity for at least `additional` more elements.
            pub fn reserve(&mut self, additional: usize) {
                self.items.reserve(additional);
            }

            /// Clears the arena, removing all values.
            ///
            /// Note: This keeps the underlying memory allocation and resets generations.
            pub fn clear(&mut self) {
                self.items.clear();
                self.free_head = None;
                self.len = 0;
            }

            /// Inserts a value into the arena, returning a unique `Index`.
            #[must_use]
            pub fn insert(&mut self, value: T) -> Index {
                self.len += 1;

                if let Some(slot) = self.free_head {
                    if let Some(Entry::Free {
                        next_free,
                        generation,
                    }) = self.items.get_mut(slot as usize)
                    {
                        let generation = *generation;
                        self.free_head = *next_free;
                        self.items[slot as usize] = Entry::Occupied { generation, value };

                        return Index { slot, generation };
                    } else {
                        panic!("Corrupted free list in Arena");
                    }
                }

                let slot = self
                    .items
                    .len()
                    .try_into()
                    .expect("Arena overflow: too many items");
                let generation = <$nonzero_type>::new(1).unwrap();
                self.items.push(Entry::Occupied { generation, value });

                Index { slot, generation }
            }

            /// Removes the value at the given index and returns it.
            /// Returns `None` if the index is invalid or the slot is empty.
            pub fn remove(&mut self, index: Index) -> Option<T> {
                let entry = self.items.get_mut(index.slot as usize)?;

                match entry {
                    Entry::Occupied { generation, .. } if *generation == index.generation => {
                        let next_gen_val = generation.get().wrapping_add(1);
                        let next_gen =
                            <$nonzero_type>::new(if next_gen_val == 0 { 1 } else { next_gen_val })
                                .unwrap();

                        let old_entry = core::mem::replace(
                            entry,
                            Entry::Free {
                                next_free: self.free_head,
                                generation: next_gen,
                            },
                        );

                        self.free_head = Some(index.slot);
                        self.len -= 1;

                        if let Entry::Occupied { value, .. } = old_entry {
                            Some(value)
                        } else {
                            unreachable!()
                        }
                    }
                    _ => None,
                }
            }

            /// Returns a reference to the value at the given index.
            pub fn get(&self, index: Index) -> Option<&T> {
                match self.items.get(index.slot as usize) {
                    Some(Entry::Occupied { generation, value })
                        if *generation == index.generation =>
                    {
                        Some(value)
                    }
                    _ => None,
                }
            }

            /// Returns a mutable reference to the value at the given index.
            pub fn get_mut(&mut self, index: Index) -> Option<&mut T> {
                match self.items.get_mut(index.slot as usize) {
                    Some(Entry::Occupied { generation, value })
                        if *generation == index.generation =>
                    {
                        Some(value)
                    }
                    _ => None,
                }
            }

            /// Returns true if the arena contains the given index.
            pub fn contains(&self, index: Index) -> bool {
                self.get(index).is_some()
            }

            /// Retrieves a value by raw slot index, ignoring generation.
            ///
            /// This is useful if you have external data referencing a slot but lost the generation info.
            /// Returns the value and the *current* valid `Index` for that slot.
            pub fn get_unknown_gen(&self, slot: $idx_type) -> Option<(&T, Index)> {
                match self.items.get(slot as usize) {
                    Some(Entry::Occupied { generation, value }) => Some((
                        value,
                        Index {
                            slot,
                            generation: *generation,
                        },
                    )),
                    _ => None,
                }
            }

            /// Iterates over all active items in the arena.
            ///
            /// Yields `(Index, &T)`.
            pub fn iter(&self) -> Iter<'_, T> {
                Iter {
                    enumerate: self.items.iter().enumerate(),
                }
            }

            /// Iterates over all active items in the arena mutably.
            ///
            /// Yields `(Index, &mut T)`.
            pub fn iter_mut(&mut self) -> IterMut<'_, T> {
                IterMut {
                    enumerate: self.items.iter_mut().enumerate(),
                }
            }

            /// Returns an iterator that removes all items from the arena.
            pub fn drain(&mut self) -> Drain<'_, T> {
                Drain {
                    arena: self,
                    slot: 0,
                }
            }
        }

        impl<T> OpsIndex<Index> for Arena<T> {
            type Output = T;
            fn index(&self, index: Index) -> &Self::Output {
                self.get(index).expect("Index is not found in Arena")
            }
        }

        impl<T> OpsIndexMut<Index> for Arena<T> {
            fn index_mut(&mut self, index: Index) -> &mut Self::Output {
                self.get_mut(index).expect("Index is not found in Arena")
            }
        }

        pub struct Iter<'a, T> {
            enumerate: core::iter::Enumerate<core::slice::Iter<'a, Entry<T>>>,
        }

        impl<'a, T> Iterator for Iter<'a, T> {
            type Item = (Index, &'a T);
            fn next(&mut self) -> Option<Self::Item> {
                for (i, entry) in self.enumerate.by_ref() {
                    if let Entry::Occupied { generation, value } = entry {
                        return Some((
                            Index {
                                slot: i as $idx_type,
                                generation: *generation,
                            },
                            value,
                        ));
                    }
                }
                None
            }
        }

        pub struct IterMut<'a, T> {
            enumerate: core::iter::Enumerate<core::slice::IterMut<'a, Entry<T>>>,
        }

        impl<'a, T> Iterator for IterMut<'a, T> {
            type Item = (Index, &'a mut T);
            // noinspection DuplicatedCode
            fn next(&mut self) -> Option<Self::Item> {
                for (i, entry) in self.enumerate.by_ref() {
                    if let Entry::Occupied { generation, value } = entry {
                        return Some((
                            Index {
                                slot: i as $idx_type,
                                generation: *generation,
                            },
                            value,
                        ));
                    }
                }
                None
            }
        }

        pub struct Drain<'a, T> {
            arena: &'a mut Arena<T>,
            slot: usize,
        }

        impl<'a, T> Iterator for Drain<'a, T> {
            type Item = (Index, T);
            fn next(&mut self) -> Option<Self::Item> {
                while self.slot < self.arena.items.len() {
                    let slot_idx = self.slot;
                    self.slot += 1;

                    if let Some(Entry::Occupied { generation, .. }) = self.arena.items.get(slot_idx)
                    {
                        let idx = Index {
                            slot: slot_idx as $idx_type,
                            generation: *generation,
                        };
                        if let Some(val) = self.arena.remove(idx) {
                            return Some((idx, val));
                        }
                    }
                }
                None
            }
        }

        impl<'a, T> Drop for Drain<'a, T> {
            fn drop(&mut self) {
                for _ in self.by_ref() {}
            }
        }

        #[cfg(feature = "serde")]
        mod serde_impl {
            use super::*;
            use serde::{Deserialize, Deserializer, Serialize, Serializer};

            impl<T: Serialize> Serialize for Arena<T> {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: Serializer,
                {
                    #[derive(serde::Serialize)]
                    struct ArenaProxy<'a, T> {
                        items: &'a Vec<Entry<T>>,
                    }

                    let proxy = ArenaProxy { items: &self.items };
                    proxy.serialize(serializer)
                }
            }

            impl<'de, T: Deserialize<'de>> Deserialize<'de> for Arena<T> {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: Deserializer<'de>,
                {
                    #[derive(serde::Deserialize)]
                    struct ArenaProxy<T> {
                        items: Vec<Entry<T>>,
                    }

                    let proxy = ArenaProxy::<T>::deserialize(deserializer)?;
                    let items = proxy.items;

                    let mut free_head = None;
                    let mut len = 0;

                    for entry in items.iter() {
                        if let Entry::Occupied { .. } = entry {
                            len += 1;
                        }
                    }

                    let mut valid_items = items;

                    for (i, entry) in valid_items.iter_mut().enumerate().rev() {
                        if let Entry::Free { next_free, .. } = entry {
                            *next_free = free_head;
                            free_head = Some(i as $idx_type);
                        }
                    }

                    Ok(Arena {
                        items: valid_items,
                        free_head,
                        len,
                    })
                }
            }
        }

        #[cfg(test)]
        mod tests {
            use super::*;

            #[test]
            fn basic_operations() {
                let mut arena = Arena::new();
                let a = arena.insert(10);
                let b = arena.insert(20);

                assert_eq!(arena[a], 10);
                assert_eq!(arena[b], 20);
                assert_eq!(arena.len(), 2);

                arena.remove(a);
                assert_eq!(arena.len(), 1);
                assert!(arena.get(a).is_none());

                let c = arena.insert(30);
                assert_eq!(c.slot(), a.slot());
                assert_ne!(c.generation(), a.generation());
                assert_eq!(arena[c], 30);
            }

            #[test]
            fn aba_protection() {
                let mut arena = Arena::new();
                let idx1 = arena.insert("Item");
                arena.remove(idx1);

                let idx2 = arena.insert("New Item");

                assert_eq!(idx1.slot(), idx2.slot());
                assert!(arena.get(idx1).is_none());
                assert_eq!(arena[idx2], "New Item");
            }

            #[test]
            #[cfg(feature = "serde")]
            fn serde_round_trip() {
                let mut arena = Arena::new();
                let a = arena.insert(1);
                let b = arena.insert(2);
                let c = arena.insert(3);

                arena.remove(b);

                let serialized = serde_json::to_string(&arena).unwrap();
                let mut arena2: Arena<i32> = serde_json::from_str(&serialized).unwrap();

                assert_eq!(arena2[a], 1);
                assert_eq!(arena2[c], 3);
                assert!(arena2.get(b).is_none());
                assert_eq!(arena2.len(), 2);

                let d = arena2.insert(4);

                assert_eq!(d.slot(), b.slot());
                assert_eq!(arena2[d], 4);
            }
        }
    };
}

#[cfg(feature = "u32")]
pub mod u32 {
    //! A 32-bit arena allocator.
    define_arena!(u32, NonZeroU32);
}

#[cfg(feature = "u64")]
pub mod u64 {
    //! A 64-bit arena allocator.
    define_arena!(u64, NonZeroU64);
}
