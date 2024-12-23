use std::ops::Deref;

/// A nonempty Vec
#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct NEVec<T>(Vec<T>);

impl<T> NEVec<T> {
    pub fn new(values: Vec<T>) -> Option<Self> {
        if values.is_empty() {
            None
        } else {
            Some(NEVec(values))
        }
    }

    pub fn single(value: T) -> Self {
        NEVec(vec![value])
    }
}

impl<T> NEVec<T> {
    pub fn last(&self) -> &T {
        self.0.last().unwrap()
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.0.last_mut().unwrap()
    }

    pub fn push(&mut self, value: T) {
        self.0.push(value)
    }

    pub fn pop_unchecked(&mut self) -> T {
        self.0.pop().unwrap()
    }
}

impl<T: Default> Default for NEVec<T> {
    fn default() -> Self {
        Self::single(Default::default())
    }
}

impl<T> Deref for NEVec<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
