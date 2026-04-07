//! Order maintenance wrapper.
//!
//! Wraps `order_maintenance::Priority` to implement `Ord` (the crate only
//! provides `PartialOrd`, returning `None` for cross-arena comparisons).
//! All priorities in this application share a single arena, so the `Ord`
//! implementation is safe.

use order_maintenance::Priority;
use std::cmp::Ordering;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub struct Order {
    priority: Priority,
}

impl Ord for Order {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl Order {
    /// Create a fresh priority in a new arena.
    pub fn new() -> Self {
        Order {
            priority: Priority::new(),
        }
    }

    /// Insert a new point immediately after this one.
    /// Returns the new point. `self` remains valid and compares less than the result.
    pub fn insert_after(&self) -> Order {
        Order {
            priority: self.priority.insert(),
        }
    }
}
