#[link(name = "our_code")]
extern "C" {
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

fn parse_input(input: &str) -> i64 {
    if input == "true" {
        3
    } else if input == "false" {
        1
    } else {
        let n: i64 = input.parse().expect("invalid input");
        if n < i32::MIN as i64 || n > i32::MAX as i64 {
            panic!("input out of range");
        }
        n << 1
    }
}

fn print_value(v: i64) {
    if v & 1 == 0 {
        println!("{}", v >> 1);
    } else if v == 3 {
        println!("true");
    } else if v == 1 {
        println!("false");
    } else {
        eprintln!("invalid argument");
        std::process::exit(1);
    }
}

#[no_mangle]
extern "C" fn snek_error(errcode: i64) {
    match errcode {
        1 => eprintln!("invalid argument"),
        2 => eprintln!("overflow"),
        _ => eprintln!("an error occurred {}", errcode),
    }
    std::process::exit(1);
}

#[no_mangle]
extern "C" fn snek_print(val: i64) -> i64 {
    print_value(val);
    val
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let input = if args.len() > 1 { parse_input(&args[1]) } else { 1 };
    let i: i64 = unsafe { our_code_starts_here(input) };
    print_value(i);
}
