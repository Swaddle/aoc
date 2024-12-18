mod days;

use days::{day01, day04, day05, day11, day13, day14, day15};

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
        5 => {
            day05::p1();
            //day05::p2();
        }
        11 => {
            day11::p1();
            //day11::p2();
        }
        12 => {
            //day12::p1();
            //day12::p2();
        }
        13 => {
            day13::p1();
            //day13::p2();
        }
        14 => {
            day14::p1();
            //day14::p2();
        }
        15 => {
            day15::p1();
            //day15::p2();
        }
        _ => println!("day not implemented"),
    }
}
