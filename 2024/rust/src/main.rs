mod days;

use days::{day01, day04};

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let day = args[1].parse::<u32>().unwrap();

    match day {
        1 => {
            day01::p1();
            day01::p2();
        }
        4 => {
            day04::p1();
            day04::p2();
        }
        _ => println!("day not implemented"),
    }
}
