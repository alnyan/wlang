extern fn printf(s: *i8, v: *i8);

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
    let array: [i8; 6] = [0i8, 0i8, 0i8, 0i8, 0i8, 0i8];
    my_memcpy(&array[0], "Hello", 5u64);

    printf("%s\n", &array[0]);
    0u64
}
