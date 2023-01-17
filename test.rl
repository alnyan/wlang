extern fn putchar(v: i32) -> i32;
extern fn getchar() -> i32;

fn put_hex_i32(v: i32) {
    let v0: i32 = v;
    let cntr: i32 = 0i32;

    while cntr != 8i32 {
        let digit: i32 = (v0 >> ((7i32 - cntr) * 4i32)) & 15i32;

        if digit == 15i32 {
            return;
        }

        if digit < 10i32 {
            putchar(digit + 48i32);
        } else {
            putchar(digit + 55i32);
        }

        cntr = cntr + 1i32;
    }
}

fn main() -> i64 {
    put_hex_i32(498i32);
    putchar(10i32);
    1
}
