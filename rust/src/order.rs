use order_maintenance::Priority;


// This wrapper exists only for the purpose of implementing the Ord trait,
// so that order maintenance elements can be used in a priority queue.
#[derive(PartialEq, Eq, Clone, PartialOrd)]
pub struct Order {
    priority: Priority
}

impl Ord for Order {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other)
            .expect("comparing Orders from different order maintenance structures")
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