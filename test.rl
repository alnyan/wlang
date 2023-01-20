extern fn printf(s: *i8, v: i64);

fn my_memcpy(dst: *i8, src: *i8, count: u64) -> *i8 {
    let d0: *i8 = dst;
    let i: u64 = 0u64;
    while i < count {
        *(dst + i) = *(src + i);
        i = i + 1u64;
    }
    dst
}

fn main() -> u64 {
    let v: i64 = 1;
    let array: [i64; 8] = [v; 8];
    array[4] = 2;
    array[5] = 2;
    array[6] = 2;
    array[7] = 2;

    let c: i64 = 0;
    while c < 8 {
        printf("%zd\n", array[c]);
        c = c + 1;
    }

    0u64
}
