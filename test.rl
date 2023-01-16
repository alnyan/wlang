static V0: u64 = 65;

extern fn putchar(v: u64) -> u64;

fn f1(x: u64) -> u64 {
    V0 + x
}

fn main() -> u64 {
    let x: u64 = 0;

    while x != 26 {
        putchar(f1(x));
        x = x + 1;
    }
    putchar(10);
    while x != 0 {
        x = x - 1;
        putchar(f1(x));
    }
    putchar(10);

    x
}
