extern fn putchar(ch: i8);
extern fn puts(s: *i8);

static X: *i8 = "test1";

fn my_puts(s: *i8) {
    let p: *i8 = s;
    while 1 == 1 {
        let c: i8 = *p;

        if c == 0i8 {
            break;
        }

        putchar(c);

        p = p + 1;
    }
    putchar(10i8);
}

fn main() -> i64 {
    my_puts(X);
    0
}
