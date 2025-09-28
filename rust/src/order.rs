use order_maintenance::Priority;
use std::cmp::Ordering;


// This wrapper exists only for the purpose of implementing the Ord trait,
// so that order maintenance elements can be used in a priority queue.
#[derive(PartialEq, Eq, Clone, PartialOrd)]
pub struct Order {
    priority: Priority
}

impl Ord for Order {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.partial_cmp(other) {
            None => Ordering::Equal,
            Some(o) => o
        }
    }
}

impl Order {
    pub fn new() -> Order {
        Order { priority : Priority::new() }
    }

    pub fn split(self) -> (Order, Order) {
        let other = Order { priority: self.priority.insert() };
        (self, other)
    }
}