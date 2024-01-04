use std::fmt;

pub fn join_fmt<T, U: fmt::Display>(slice: &[T], mut map: impl FnMut(&T) -> U) -> String {
    let mut iter = slice.iter();
    let first = match iter.next() {
        Some(first) => first,
        None => return String::new(),
    };

    let mut fmt = |t| map(t).to_string();
    let mut place = String::with_capacity(128);

    place.push_str(&fmt(first));
    for t in iter {
        place.push_str(", ");
        place.push_str(&fmt(t));
    }
    place
}

pub fn join_fmt_debug<T: fmt::Debug>(slice: &[T]) -> String {
    let fmt = |t| format!("{:?}", t);

    let mut iter = slice.iter();
    let first = match iter.next() {
        Some(first) => first,
        None => return String::new(),
    };

    let mut fmt = |t| fmt(t).to_string();
    let mut place = String::with_capacity(128);

    place.push_str(&fmt(first));
    for t in iter {
        place.push_str(", ");
        place.push_str(&fmt(t));
    }
    place
}
