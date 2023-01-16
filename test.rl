static V0: u64 = 65;

extern fn putchar(v: u64) -> u64;

fn f1(x: u64) -> u64 {
    V0 + x
}

fn main() -> u64 {
    putchar(f1(0));
    putchar(f1(1));
    putchar(f1(2));
    putchar(f1(3));

    0
}
