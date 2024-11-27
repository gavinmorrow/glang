pub struct Stream<T> {
    data: Vec<T>,
    pointer_to_next: usize,
}

impl<T> Stream<T> {
    pub fn new(data: Vec<T>) -> Self {
        Stream {
            data,
            pointer_to_next: 0,
        }
    }
}

impl<T: Clone> Iterator for Stream<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.data.get(self.pointer_to_next).cloned();
        self.pointer_to_next += 1;
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

    // TODO: maybe return an iterator?
    pub fn next_while(&mut self, f: impl Fn(&T) -> bool) -> Vec<T> {
        let mut items = vec![];
        while let Some(next) = self.peek() {
            if f(next) {
                let value = self.next().expect("iterator has next item");
                items.push(value);
            } else {
                break;
            }
        }
        items
    }
}

impl<T> Stream<T> {
    pub fn advance(&mut self) -> Option<usize> {
        if self.pointer_to_next + 1 >= self.data.len() {
            None
        } else {
            self.pointer_to_next += 1;
            Some(self.pointer_to_next)
        }
    }

    pub fn peek(&self) -> Option<&T> {
        self.peek_many::<0>()
    }

    // Use a const param b/c dynamic lookahead feels like a bad idea.
    /// Peek ahead of the next element.
    ///
    /// `N` is the amount to peek ahead of the next element.
    /// e.g. 0 -> peeks next element.
    ///      1 -> peeks the element after the next one.
    pub fn peek_many<const N: usize>(&self) -> Option<&T> {
        self.data.get(self.pointer_to_next + N)
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
