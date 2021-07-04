use crate::types::TypeStorage;

pub struct DisplayType<'i, T> {
    pub storage: &'i TypeStorage<'i>,
    pub ty: &'i T,
}

impl<'i, T: 'i> DisplayType<'i, T> {
    pub fn new(ty: &'i T, storage: &'i TypeStorage<'i>) -> Self {
        DisplayType { storage, ty }
    }

    pub fn display<U>(&self, other: &'i U) -> DisplayType<'i, U> {
        DisplayType {
            storage: self.storage,
            ty: other,
        }
    }
}
