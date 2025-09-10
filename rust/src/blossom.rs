use core::num;

use priority_queue::PriorityQueue;

use crate::grove;
use grove::Node;
use grove::NodeMap;


pub struct State {
    grove : grove::State,
    nodecount : NodeMap<u32>,
    worklist : PriorityQueue<Node, u128>
}

pub type Patch = grove::Patch;

impl State {

    // how to restrict to term analysis instead of graph?
    fn correct_nodecount(&mut self, n : Node) {
        let num_children = grove::State::num_children_of_term(&self.grove, &n);
        let mut total = 1; 
        for child in 0..num_children {
            let cs = grove::State::term_children_of_location(&self.grove, l);
            todo!()
        }
        self.nodecount.insert(n, total);
    }

    pub fn propagate_step(&mut self) -> Option<()> {
        let (n, _) = self.worklist.pop()?;
        self.correct_nodecount(n);
        Some(())
    }

    pub fn apply_patch(&mut self, p : Patch) {
        let budged = self.grove.apply_patch(p);
        for n in budged {
            self.worklist.push(n, 0);
        }
    }
}