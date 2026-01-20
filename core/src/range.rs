use core::{
    cmp::Ordering,
    fmt,
    ops::{Add, AddAssign, Bound, Index, IndexMut, RangeBounds, Sub},
};

pub trait NumericRangeBound:
    Add<Output = Self> + AddAssign + Sub<Output = Self> + Copy + PartialEq + Ord
{
    const ZERO: Self;
    const ONE: Self;

    fn saturating_sub(self, rhs: Self) -> Self;
}

macro_rules! numeric_bound_impl {
    ($type:ty) => {
        impl NumericRangeBound for $type {
            const ZERO: Self = 0;
            const ONE: Self = 0;

            #[inline(always)]
            fn saturating_sub(self, rhs: Self) -> Self {
                <$type>::saturating_sub(self, rhs)
            }
        }
    };
}

numeric_bound_impl!(usize);
numeric_bound_impl!(u8);
numeric_bound_impl!(u16);
numeric_bound_impl!(u32);
numeric_bound_impl!(u64);

pub struct Range<T> {
    pub start: T,
    pub end: T,
}
impl<T: fmt::Debug> fmt::Debug for Range<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Range")
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}
impl<T: fmt::Display> fmt::Display for Range<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", &self.start, &self.end)
    }
}
impl<T> core::ops::RangeBounds<T> for Range<T> {
    #[inline]
    fn start_bound(&self) -> Bound<&T> {
        Bound::Included(&self.start)
    }
    #[inline]
    fn end_bound(&self) -> Bound<&T> {
        Bound::Excluded(&self.end)
    }
}
impl<T: PartialOrd + fmt::Debug> Range<T> {
    pub fn new(start: T, end: T) -> Self {
        assert!(
            start <= end,
            "invalid range: start {:?} must not be larger than end {:?}",
            start,
            end,
        );
        Self { start, end }
    }
}
impl<T: PartialOrd> Range<T> {
    #[inline]
    pub fn into_range(self) -> core::ops::Range<T> {
        self.start..self.end
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        !matches!(self.start.partial_cmp(&self.end), Some(Ordering::Less))
    }

    #[inline]
    pub fn contains<U>(&self, item: &U) -> bool
    where
        T: PartialOrd<U>,
        U: PartialOrd<T> + ?Sized,
    {
        <Self as core::ops::RangeBounds<T>>::contains(self, item)
    }
}
impl<T: NumericRangeBound> Range<T> {
    #[inline(always)]
    pub fn len(&self) -> T {
        self.end - self.start
    }

    /// Shrink the length of this range by one element from the front, returning the first item in the range.
    #[inline]
    pub fn pop_front(&mut self) -> Option<T> {
        let item = self.start;
        let next = item + <T as NumericRangeBound>::ONE;
        if next <= self.end {
            self.start = next;
            Some(item)
        } else {
            None
        }
    }

    /// Shrink the length of this range by one element from the end, returning the last item in the range.
    #[inline]
    pub fn pop_back(&mut self) -> Option<T> {
        if self.start == self.end {
            None
        } else {
            let item = self.end.saturating_sub(<T as NumericRangeBound>::ONE);
            self.end = item;
            Some(item)
        }
    }

    /// Shrink this range by advancing the start of the range `n` elements, i.e. `(start + n)..end`
    ///
    /// NOTE: This function will saturate to `self.end` to ensure the range remains valid.
    #[inline]
    pub fn shrink_front(&mut self, n: T) {
        self.start = core::cmp::min(self.start + n, self.end);
    }

    /// Truncate this range such that it's length is `new_len`, i.e. `start..(start + new_len)`
    ///
    /// This effectively drops elements from the back of the range;
    ///
    /// NOTE: This function will panic if `new_len` is greater than the current length
    #[inline]
    pub fn truncate(&mut self, new_len: T) {
        assert!(self.len() > new_len);
        self.end = self.start + new_len;
    }
}
impl<T: Copy> Copy for Range<T> {}
impl<T: Clone> Clone for Range<T> {
    fn clone(&self) -> Self {
        Self {
            start: self.start.clone(),
            end: self.end.clone(),
        }
    }
}
impl<T: Eq> Eq for Range<T> {}
impl<T: PartialEq> PartialEq for Range<T> {
    fn eq(&self, other: &Self) -> bool {
        self.start.eq(&other.start) && self.end.eq(&other.end)
    }
}
impl<T: PartialOrd> PartialOrd for Range<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.start.partial_cmp(&other.start).and_then(|o| match o {
            Ordering::Equal => self.end.partial_cmp(&other.end),
            o => Some(o),
        })
    }
}
impl<T: Ord> Ord for Range<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start
            .cmp(&other.start)
            .then_with(|| self.end.cmp(&other.end))
    }
}
impl<T: NumericRangeBound> From<core::ops::Range<T>> for Range<T> {
    #[inline(always)]
    fn from(range: core::ops::Range<T>) -> Self {
        debug_assert!(
            range.end >= range.start,
            "invalid range: start must not pass end"
        );
        Self {
            start: range.start,
            end: range.end,
        }
    }
}
impl<T: NumericRangeBound> From<Range<T>> for core::ops::Range<T> {
    #[inline(always)]
    fn from(range: Range<T>) -> core::ops::Range<T> {
        range.start..range.end
    }
}
impl<T> Index<Range<usize>> for [T] {
    type Output = [T];

    #[inline(always)]
    fn index(&self, idx: Range<usize>) -> &Self::Output {
        <[T] as Index<core::ops::Range<usize>>>::index(self, idx.start..idx.end)
    }
}
impl<T> IndexMut<Range<usize>> for [T] {
    #[inline(always)]
    fn index_mut(&mut self, idx: Range<usize>) -> &mut Self::Output {
        <[T] as IndexMut<core::ops::Range<usize>>>::index_mut(self, idx.start..idx.end)
    }
}

pub fn range_from_bounds<R: RangeBounds<usize>>(
    range: R,
    allowed: Range<usize>,
) -> Result<Range<usize>, Range<usize>> {
    let start = match range.start_bound() {
        Bound::Included(&i) => i,
        Bound::Excluded(&i) => i + 1,
        Bound::Unbounded => allowed.start,
    };
    let end = match range.end_bound() {
        Bound::Included(&i) => i + 1,
        Bound::Excluded(&i) => i,
        Bound::Unbounded => allowed.end,
    };
    if start < allowed.start || end > allowed.end {
        Err(Range::new(start, end))
    } else {
        Ok(Range::new(start, end))
    }
}

pub fn range_from_bounds_with_defaults<R: RangeBounds<usize>>(
    range: R,
    default_start: usize,
    default_end: usize,
) -> Range<usize> {
    let start = match range.start_bound() {
        Bound::Included(&i) => i,
        Bound::Excluded(&i) => i + 1,
        Bound::Unbounded => default_start,
    };
    let end = match range.end_bound() {
        Bound::Included(&i) => i + 1,
        Bound::Excluded(&i) => i,
        Bound::Unbounded => default_end,
    };
    Range::new(start, end)
}
