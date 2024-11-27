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

impl<T: Clone> Stream<T> {
    pub fn next_if(&mut self, f: impl FnOnce(&T) -> bool) -> Option<T> {
        match self.peek() {
            Some(x) if f(x) => {
                let x = self.next().unwrap();
                Some(x)
            }
            _ => None,
        }
    }

    pub fn next_while(&mut self, f: impl Fn(&T) -> bool) -> Vec<T> {
        let mut items = vec![];
        while let Some(next) = self.peek() {
            if f(next) {
                items.push(self.next().expect("iterator has next item"));
            } else {
                break;
            }
        }
        items
    }
}

impl<T> Stream<T> {
    pub fn advance(&mut self) -> Option<usize> {
        if self.pointer >= self.data.len() - 1 {
            None
        } else {
            self.pointer += 1;
            Some(self.pointer)
        }
    }

    pub fn peek(&self) -> Option<&T> {
        self.peek_many::<1>()
    }

    // Use a const param b/c dynamic lookahead feels like a bad idea.
    pub fn peek_many<const N: usize>(&self) -> Option<&T> {
        self.data.get(self.pointer + N)
    }

    pub fn advance_while(&mut self, f: impl Fn(&T) -> bool) -> usize {
        let mut num_advanced = 0;

        while let Some(next) = self.peek() {
            if f(next) {
                self.advance().expect("iterator has next item");
                num_advanced += 1;
            } else {
                break;
            }
        }

        num_advanced
    }
}
