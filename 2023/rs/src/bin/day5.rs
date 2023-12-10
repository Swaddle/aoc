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

type Transformation = (u64, u64, u64);

fn gen_affine_transform(transformations: Vec<Transformation>) -> impl Fn(u64) -> u64 {
    move |source_value| {
        transformations.iter().find_map(|&(target_range_start, source_range_start, n_steps)| {
            let source_range_end = source_range_start + n_steps as u64;
            let target_range_end = target_range_start + n_steps as u64;
            if source_value >= source_range_start && source_value < source_range_end {
                Some(target_range_start + (source_value - source_range_start))
            } else {
                None
            }
        }).unwrap_or(source_value)
    }
}


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


fn main() {
    // --snip--

    let transformations = vec![(50, 98, 2), (30, 70, 3)]; // Example transformations
    let affine_transform = gen_affine_transform(transformations);
    let source_values = vec![99, 65, 40];

    for source_value in vec![99, 98, 65, 40] {
        println!("{} -> {}", source_value, affine_transform(source_value));
    }


    // let contents = fs::read_to_string("../data/5.txt").unwrap();
    // let (remaining, (_label, seeds)) = parse_seeds(&contents).unwrap();
    // println!("{:?}", seeds);
    // println!("{}", remaining)
}