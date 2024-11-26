pub struct Stream<T> {
    data: Vec<T>,
    pointer: usize,
}

impl<T> Stream<T> {
    pub fn new(data: Vec<T>) -> Self {
        Stream { data, pointer: 0 }
    }
}

impl<T: Clone> Iterator for Stream<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.data.get(self.pointer).cloned();
        self.pointer += 1;
        item
    }
}

impl<T> Stream<T> {
    pub fn peek(&self) -> Option<&T> {
        self.data.get(self.pointer + 1)
    }
}
