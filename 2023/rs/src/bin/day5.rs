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
    branch::alt,
    multi::many1
};

type Transformation = (u64, u64, u64);

fn gen_affine_transform(transformations: &Vec<Transformation>) -> impl Fn(u64) -> u64 + '_ {
    move |source_value| {
        transformations.iter().find_map(|&(target_range_start, source_range_start, n_steps)| {
            let source_range_end = source_range_start + n_steps as u64;
            if source_value >= source_range_start && source_value < source_range_end {
                Some(target_range_start + (source_value - source_range_start))
            } else {
                None
            }
        }).unwrap_or(source_value)
    }
}

fn parse_transform_row(input: &str) -> IResult<&str, Transformation> {
    let mut parse_values = separated_list1(
        tag(" "),
        map_res(digit1, u64::from_str)
    );
    let (rem, vec) = parse_values(input)?;
    Ok((rem,(vec[0],vec[1],vec[2])))
}

fn parse_transform(input: &str) -> IResult<&str, Vec<Transformation>> {
    let(rem, (_label,_,transforms,_)) = tuple((
        parse_label,
        line_ending,
        separated_list1(
            line_ending,
            parse_transform_row
        ),
        line_ending
    ))(input)?;
    Ok((rem, transforms))
}

fn parse_transforms(input: &str) -> IResult<&str, Vec<Vec<Transformation>>> {
    many1(parse_transform)(input)
}

fn parse_label(input: &str) -> IResult<&str, &str> {
    alt((
        tag("seed-to-soil map:"),
        tag("soil-to-fertilizer map:"),
        tag("fertilizer-to-water map:"), 
        tag("water-to-light map:"),
        tag("light-to-temperature map:"), 
        tag("temperature-to-humidity map:"), 
        tag("humidity-to-location map:")  
    ))(input)
}


fn parse_seeds(input: &str) -> IResult<&str, Vec<u64>> {
    let (remainder, _label) = tag("seeds: ")(input)?;
    //let (remainder, seeds) = separa ted_list1(tag(" "), map_res(digit1, u64::from_str))(remainder)?;
    let (remainder, (seeds, _,_)) =  tuple((
        separated_list1(tag(" "), map_res(digit1, u64::from_str)),
        line_ending,
        line_ending,
    ))(remainder)?;
    Ok((remainder, seeds))
}


fn main() {
    // --snip--

    // let transformations = vec![(50, 98, 2), (30, 70, 3)]; // Example transformations
    // let affine_transform = gen_affine_transform(transformations);
    // let source_values = vec![99, 65, 40];

    // for source_value in vec![99, 98, 65, 40] {
    //     println!("{} -> {}", source_value, affine_transform(source_value));
    // }

    let contents = fs::read_to_string("../data/5.txt").unwrap();
    let (remaining, seeds) = parse_seeds(&contents).unwrap();
    println!("{:?}", seeds);
    let (_remaining, transforms) = parse_transforms(&remaining).unwrap();
    println!("{:?}", transforms);

    for seed in &seeds {
        for transform in &transforms {
            let affine_transform = gen_affine_transform(transform);
            println!("{} -> {}", seed, affine_transform(*seed));
        }
    }

}