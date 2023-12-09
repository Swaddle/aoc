use std::fs;
use std::str::FromStr;
use nom::{
    IResult,
    bytes::complete::tag,
    multi::separated_list1,
    sequence::tuple,
    character::complete::digit1,
    combinator::map_res,
    character::complete::line_ending,
};


fn parse_seeds(input: &str) ->  IResult<&str, (&str, Vec<u64>) > {
    tuple(
        (
        tag("seeds: "),
        separated_list1(
            tag(" "),
            map_res(digit1, u64::from_str)
        )
    )
    )(input)
}

// pub fn parse_seeds(input: &str) ->  IResult<&str, Vec<u64>> {
//     // nom parser to get a row of text from a string w format eg:
//     // seeds:  2 3 4 

// }


fn main() {
    // --snip--
    let contents = fs::read_to_string("../data/5.txt").unwrap();
    let (remaining, (_label, seeds)) = parse_seeds(&contents).unwrap();
    println!("{:?}", seeds);
    println!("{}", remaining)
}