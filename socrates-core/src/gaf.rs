use std::cell::RefCell;
use std::rc::Rc;

use fnv::FnvHashMap;

static SLACK_NAME: &str = "__slack__";

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct GAFIndex(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum GroundTerm<'i> {
    Named(&'i str, Vec<GroundTerm<'i>>),
    Number(i64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GAF<'i> {
    name: &'i str,
    args: Vec<GroundTerm<'i>>,
}

#[derive(Debug, Default)]
pub struct GAFStorageInner<'i> {
    gaf_lookup: FnvHashMap<GAF<'i>, GAFIndex>,
    slack_counter: i64,
}

#[derive(Debug, Default, Clone)]
pub struct GAFStorage<'i> {
    inner: Rc<RefCell<GAFStorageInner<'i>>>,
}

impl<'i> GAFStorage<'i> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn reify(&self, gaf: GAF<'i>) -> GAFIndex {
        let mut inner = self.inner.borrow_mut();
        let next_index = GAFIndex(inner.gaf_lookup.len() as u32);
        *inner.gaf_lookup.entry(gaf).or_insert(next_index)
    }

    pub fn next_slack(&self) -> GAFIndex {
        let gaf;
        {
            let mut inner = self.inner.borrow_mut();
            inner.slack_counter += 1;
            let arg = GroundTerm::Number(inner.slack_counter);
            gaf = GAF::new(SLACK_NAME, vec![arg]);
        }
        self.reify(gaf)
    }
}

impl<'i> GroundTerm<'i> {
    pub fn new_ident(name: &'i str) -> Self {
        GroundTerm::Named(name, vec![])
    }

    pub fn new_function(name: &'i str, args: Vec<GroundTerm<'i>>) -> Self {
        GroundTerm::Named(name, args)
    }
}

impl<'i> GAF<'i> {
    pub fn new(name: &'i str, args: Vec<GroundTerm<'i>>) -> Self {
        GAF { name, args }
    }
}
