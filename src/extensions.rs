use std::cell::Cell;
use std::num::One;

pub trait Incrementor {
    fn increment(&self);
}

impl<T: Copy + Num> Incrementor for Cell<T> {
    fn increment(&self) {
        self.set(self.get() + One::one());
    }
}
