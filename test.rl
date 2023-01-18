extern fn putchar(ch: i8);

fn main() -> i64 {
    let a: [i8; 13] = [
        104i8,
        101i8,
        108i8,
        108i8,
        111i8,
        44i8,
        32i8,
        119i8,
        111i8,
        114i8,
        108i8,
        100i8,
        10i8
    ];

    let index: i64 = 0;
    while index < 13 {
        putchar(a[index]);
        index = index + 1;
    }

    while index != 0 {
        index = index - 1;
        putchar(a[index]);
    }

    0
}
