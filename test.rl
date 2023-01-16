static V0: u64 = 64;

fn main() -> u64 {
    let x: u64 = 1;
    f1(x + 1) + V0
}

fn f1(x: u64) -> u64 {
    x + 1
}
