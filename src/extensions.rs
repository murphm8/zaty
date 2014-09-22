use std::cell::Cell;

pub trait Incrementor {
    fn increment(&self);
}

impl Incrementor for Cell<u8> {
    fn increment(&self) {
        self.set(self.get() + 1);
    }
}

impl Incrementor for Cell<u16> {
    fn increment(&self) {
        self.set(self.get() + 1);
    }
}
