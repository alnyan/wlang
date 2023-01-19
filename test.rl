extern fn putchar(ch: i8);
extern fn puts(s: *i8);

fn main() -> i64 {
    let x: [i8; 6] = [
        104i8,
        101i8,
        108i8,
        108i8,
        111i8,
        0i8
    ];
    puts(&x[0]);
    0
}
