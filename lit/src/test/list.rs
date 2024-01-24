use std::sync::Arc;

use intrusive_collections::{intrusive_adapter, LinkedList, LinkedListAtomicLink};

use super::Test;

pub type TestCursor<'a> = intrusive_collections::linked_list::Cursor<'a, TestAdapter>;

intrusive_adapter!(pub TestAdapter = Arc<Test>: Test { link: LinkedListAtomicLink });

#[derive(Default)]
pub struct TestList {
    len: usize,
    tests: LinkedList<TestAdapter>,
}
impl TestList {
    pub fn is_empty(&self) -> bool {
        self.tests.is_empty()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    #[allow(unused)]
    pub fn front(&self) -> TestCursor<'_> {
        self.tests.front()
    }

    #[allow(unused)]
    pub fn back(&self) -> TestCursor<'_> {
        self.tests.back()
    }

    pub fn front_mut(&mut self) -> TestCursorMut<'_> {
        TestCursorMut {
            len: &mut self.len,
            pos: 0,
            tests: self.tests.front_mut(),
        }
    }

    #[allow(unused)]
    pub fn back_mut(&mut self) -> TestCursorMut<'_> {
        TestCursorMut {
            len: &mut self.len,
            pos: 0,
            tests: self.tests.back_mut(),
        }
    }

    pub fn pop_front(&mut self) -> Option<Arc<Test>> {
        let test = self.tests.pop_front();
        if test.is_some() {
            self.len -= 1;
        }
        test
    }

    pub fn pop_back(&mut self) -> Option<Arc<Test>> {
        let test = self.tests.pop_back();
        if test.is_some() {
            self.len -= 1;
        }
        test
    }

    pub fn clear(&mut self) {
        self.len = 0;
        self.tests.clear();
    }

    pub fn iter(&self) -> Iter<'_> {
        Iter::new(self.len, self.tests.front())
    }

    pub fn push_back(&mut self, test: Arc<Test>) {
        self.len += 1;
        self.tests.push_back(test);
    }

    pub fn append(&mut self, tests: Self) {
        self.len += tests.len;
        self.tests.back_mut().splice_after(tests.tests);
    }
}

pub struct TestCursorMut<'a> {
    len: &'a mut usize,
    pos: usize,
    tests: intrusive_collections::linked_list::CursorMut<'a, TestAdapter>,
}
impl<'a> TestCursorMut<'a> {
    pub fn is_null(&self) -> bool {
        self.tests.is_null()
    }

    #[allow(unused)]
    pub fn get(&self) -> Option<&Test> {
        self.tests.get()
    }

    pub fn move_next(&mut self) {
        if self.pos == *self.len {
            assert!(self.tests.is_null());
            self.tests.move_next();
            self.pos = 0;
        } else {
            self.tests.move_next();
            self.pos += 1;
        }
    }

    #[allow(unused)]
    pub fn move_prev(&mut self) {
        if self.pos == *self.len {
            assert!(self.tests.is_null());
            self.tests.move_prev();
            self.pos = self.len.saturating_sub(1);
        } else if self.pos == 0 {
            self.tests.move_prev();
            self.pos = *self.len;
        } else {
            self.tests.move_prev();
            self.pos -= 1;
        }
    }

    #[allow(unused)]
    pub fn remove(&mut self) -> Option<Arc<Test>> {
        let test = self.tests.remove();
        if test.is_some() {
            *self.len -= 1;
        }
        test
    }

    #[allow(unused)]
    pub fn insert_after(&mut self, test: Arc<Test>) {
        if self.pos == *self.len {
            self.pos += 1;
        }
        *self.len += 1;
        self.tests.insert_after(test);
    }

    #[allow(unused)]
    pub fn insert_before(&mut self, test: Arc<Test>) {
        *self.len += 1;
        self.pos += 1;
        self.tests.insert_before(test);
    }

    pub fn split_after(&mut self) -> TestList {
        let len = *self.len;
        if len == 0 {
            TestList::default()
        } else if self.pos == len {
            let tests = self.tests.split_after();
            *self.len = 0;
            self.pos = 0;
            TestList { len, tests }
        } else {
            let tests = self.tests.split_after();
            let new_len = self.pos + 1;
            *self.len = new_len;
            TestList {
                len: len - new_len,
                tests,
            }
        }
    }

    #[allow(unused)]
    pub fn splice_after(&mut self, list: TestList) {
        if self.pos == *self.len {
            self.pos += 1;
        }
        *self.len += list.len;
        self.tests.splice_after(list.tests);
    }

    #[allow(unused)]
    pub fn splice_before(&mut self, list: TestList) {
        self.pos += 1;
        *self.len += list.len;
        self.tests.splice_before(list.tests);
    }
}
impl IntoIterator for TestList {
    type Item = Arc<Test>;
    type IntoIter = Drain;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        Drain::new(self)
    }
}
impl rayon::iter::IntoParallelIterator for TestList {
    type Item = Arc<Test>;
    type Iter = Drain;

    #[inline]
    fn into_par_iter(self) -> Self::Iter {
        Drain::new(self)
    }
}

pub struct Iter<'a> {
    range: core::ops::Range<usize>,
    cursor: TestCursor<'a>,
}
impl<'a> Iter<'a> {
    fn new(len: usize, cursor: TestCursor<'a>) -> Self {
        Self {
            range: 0..len,
            cursor,
        }
    }
}
impl<'a> core::iter::FusedIterator for Iter<'a> {}
impl<'a> ExactSizeIterator for Iter<'a> {
    #[inline(always)]
    fn len(&self) -> usize {
        self.range.len()
    }
}
impl<'a> Iterator for Iter<'a> {
    type Item = Arc<Test>;

    fn next(&mut self) -> Option<Self::Item> {
        self.range.next()?;
        let test = self.cursor.clone_pointer();
        self.cursor.move_next();
        test
    }
}
impl<'a> DoubleEndedIterator for Iter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.range.next_back()?;
        let test = self.cursor.clone_pointer();
        self.cursor.move_prev();
        test
    }
}

pub struct Drain {
    tests: TestList,
}
impl Drain {
    pub fn new(tests: TestList) -> Self {
        Self { tests }
    }
}
impl core::iter::FusedIterator for Drain {}
impl ExactSizeIterator for Drain {
    #[inline(always)]
    fn len(&self) -> usize {
        self.tests.len()
    }
}
impl Iterator for Drain {
    type Item = Arc<Test>;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.tests.pop_front()
    }
}
impl DoubleEndedIterator for Drain {
    #[inline(always)]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.tests.pop_back()
    }
}
impl rayon::iter::plumbing::Producer for Drain {
    type Item = Arc<Test>;
    type IntoIter = Drain;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self
    }

    fn split_at(mut self, index: usize) -> (Self, Self) {
        if self.tests.is_empty() {
            return (self, Drain::new(Default::default()));
        }
        assert!(index < self.tests.len());
        let mut cursor = self.tests.front_mut();
        for _ in 0..index {
            cursor.move_next();
        }
        assert!(!cursor.is_null());
        let split = cursor.split_after();
        (self, Self { tests: split })
    }
}
impl rayon::iter::plumbing::UnindexedProducer for Drain {
    type Item = Arc<Test>;

    fn split(self) -> (Self, Option<Self>) {
        use rayon::iter::plumbing::Producer;

        let len = self.tests.len();
        if len < 2 {
            return (self, None);
        }
        let (a, b) = self.split_at(len / 2);
        (a, Some(b))
    }

    fn fold_with<F>(self, folder: F) -> F
    where
        F: rayon::iter::plumbing::Folder<Self::Item>,
    {
        folder.consume_iter(self)
    }
}
impl rayon::iter::ParallelIterator for Drain {
    type Item = Arc<Test>;

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: rayon::iter::plumbing::UnindexedConsumer<Self::Item>,
    {
        rayon::iter::plumbing::bridge(self, consumer)
    }

    #[inline(always)]
    fn opt_len(&self) -> Option<usize> {
        Some(self.tests.len())
    }
}
impl rayon::iter::IndexedParallelIterator for Drain {
    fn drive<C>(self, consumer: C) -> C::Result
    where
        C: rayon::iter::plumbing::Consumer<Self::Item>,
    {
        rayon::iter::plumbing::bridge(self, consumer)
    }

    #[inline(always)]
    fn len(&self) -> usize {
        self.tests.len()
    }

    fn with_producer<CB>(self, callback: CB) -> CB::Output
    where
        CB: rayon::iter::plumbing::ProducerCallback<Self::Item>,
    {
        callback.callback(self)
    }
}
