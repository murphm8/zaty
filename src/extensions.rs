use std::cell::Cell;
use num::{Signed};

pub trait Incrementor {
    fn increment(&self);
}

impl<T: Copy + Signed> Incrementor for Cell<T> {
    fn increment(&self) {
        self.set(self.get() + T::one());
    }
}
