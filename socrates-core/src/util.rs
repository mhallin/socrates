pub fn plural(num: usize) -> &'static str {
    if num == 1 {
        ""
    } else {
        "s"
    }
}
