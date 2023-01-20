extern fn printf(s: *i8, v: *i8);

fn main(argc: i64, argv: **i8) -> u64 {
    let args: * *i8 = argv;
    let counter: i64 = 0;

    while counter < argc {
        let arg_addr: u64 = (args as u64) + ((counter as u64) * 8u64);
        let arg: *i8 = *(arg_addr as **i8);

        printf("%s\n", arg);

        counter = counter + 1;
    }

    argc as u64
}
