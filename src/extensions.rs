use std::cell::Cell;
use std::num::{Int};

pub trait Incrementor {
    fn increment(&self);
}

impl<T: Copy + Int> Incrementor for Cell<T> {
    fn increment(&self) {
        self.set(self.get() + Int::one());
    }
}
