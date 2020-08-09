// like std::ops::Range, but implements Copy instead of iterator traits
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct Range<Idx> {
    pub start: Idx,
    pub end: Idx,
}
