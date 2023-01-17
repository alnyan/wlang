extern fn putchar(v: i32) -> i32;
extern fn getchar() -> i32;

fn main() -> i64 {
    let x: u32 = 1u32;
    let y: u32 = 0u32;
    while x == 1u32 && y == 0u32 {
        putchar(113i32);
        putchar(10i32);
        break;
    }
    1
}
