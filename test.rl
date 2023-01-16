extern fn putchar(v: i32) -> i32;
extern fn getchar() -> i32;

fn main() -> i64 {
    let should_exit: i32 = 0i32;
    while should_exit == 0i32 {
        let ch: i32 = getchar();
        putchar(ch);

        if ch == 113i32 {
            should_exit = 1i32;
        }
    }
    1
}
