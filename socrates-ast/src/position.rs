use std::ops::{Deref, DerefMut};

pub type Span = (usize, usize);

#[derive(Copy, Clone, Debug)]
pub struct Spanning<T> {
    pub pos: Span,
    pub inner: T,
}

impl<T> Deref for Spanning<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> DerefMut for Spanning<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

impl<T> Spanning<T> {
    pub fn as_ref(&self) -> Spanning<&T> {
        Spanning {
            pos: self.pos,
            inner: &self.inner,
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanning<U> {
        Spanning {
            pos: self.pos,
            inner: f(self.inner),
        }
    }
}
