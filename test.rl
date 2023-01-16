static V0: u64 = 64;

fn main() -> u64 {
    let x: u64 = 1;
    let y: u64 = x + {
        let x: u64 = 3;
        x + 1
    };

    x + y + V0
}
