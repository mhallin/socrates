use std::sync::{Arc, atomic::AtomicUsize, atomic::Ordering};

pub struct Scope<T> {
    pub self_index: usize,
    counter: Arc<AtomicUsize>,
    parent: Option<Arc<Scope<T>>>,
    pub inner: T,
}

impl<T> Scope<T> {
    pub fn new_toplevel(inner: T) -> Arc<Self> {
        Arc::new(Scope {
            self_index: 0,
            counter: Arc::new(AtomicUsize::new(0)),
            parent: None,
            inner,
        })
    }

    pub fn make_subscope(self_: &Arc<Scope<T>>, inner: T) -> Arc<Scope<T>> {
        let idx = self_.counter.fetch_add(1, Ordering::Relaxed);
        Arc::new(Scope {
            self_index: idx,
            counter: self_.counter.clone(),
            parent: Some(self_.clone()),
            inner,
        })
    }

    pub fn parent(&self) -> Option<&Scope<T>> {
        self.parent.as_ref().map(|a| a.as_ref())
    }
}
