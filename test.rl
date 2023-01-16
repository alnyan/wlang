extern fn putchar(v: i32) -> i32;
extern fn getchar() -> i32;

fn main() -> i64 {
    while 1 == 1 {
        let ch: i32 = getchar();
        putchar(ch);

        if ch == 113i32 {
            break;
        }
    }
    1
}
