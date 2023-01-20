extern fn printf(s: *i8, v: i64);

fn main() -> u64 {
    let y: i64 = 1;
    let x: i64 = if y != 0 {
        1
    } else {
        2
    };

    0u64
}
