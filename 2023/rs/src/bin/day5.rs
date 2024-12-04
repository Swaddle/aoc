use nom::{
    branch::alt, bytes::complete::tag, character::complete::digit1,
    character::complete::line_ending, combinator::map_res, multi::many1, multi::separated_list1,
    sequence::tuple, IResult,
};
use std::fs;
use std::str::FromStr;

type Transformation = (i64, i64, i64);

fn gen_affine_transform(transformations: &Vec<Transformation>) -> impl Fn(i64) -> i64 + '_ {
    move |source_value| {
        transformations
            .iter()
            .find_map(|&(target_range_start, source_range_start, n_steps)| {
                let source_range_end = source_range_start + n_steps as i64;
                if source_value >= source_range_start && source_value < source_range_end {
                    Some(source_value + (target_range_start - source_range_start))
                } else {
                    None
                }
            })
            .unwrap_or(source_value)
    }
}

// Now take a source range, and return a list of output ranges, where some are the identity
fn gen_split_affine_transform(
    trasformations: &Vec<Transformation>,
) -> impl Fn(i64, i64) -> Vec<(i64, i64)> + '_ {
    move |a, b| {
        let mut ranges = vec![];

        for transform in trasformations {
            let temp = vec![*transform];
            let affine_transform = gen_affine_transform(&temp);
            // need to check if the source transform acts as the identity
            // with binary search
            let f = move |x| affine_transform(x) == x;
            let mid = search(a, b, f);
            ranges.push((a, mid));
            ranges.push((mid + 1, b));
        }
        ranges
    }
}

fn search(x: i64, y: i64, f: impl Fn(i64) -> bool) -> i64 {
    if x == y {
        return x;
    }
    let mid = (x + y) / 2;
    if f(mid) {
        search(x, mid, f)
    } else {
        search(mid + 1, y, f)
    }
}

fn parse_transform_row(input: &str) -> IResult<&str, Transformation> {
    let mut parse_values = separated_list1(tag(" "), map_res(digit1, i64::from_str));
    let (rem, vec) = parse_values(input)?;
    Ok((rem, (vec[0], vec[1], vec[2])))
}

fn parse_transform(input: &str) -> IResult<&str, Vec<Transformation>> {
    let (rem, (_label, _, transforms, _, _)) = tuple((
        parse_label,
        line_ending,
        separated_list1(line_ending, parse_transform_row),
        line_ending,
        line_ending,
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
        tag("humidity-to-location map:"),
    ))(input)
}

fn parse_seeds_part1(input: &str) -> IResult<&str, Vec<i64>> {
    let (remainder, _label) = tag("seeds: ")(input)?;
    //let (remainder, seeds) = separa ted_list1(tag(" "), map_res(digit1, i64::from_str))(remainder)?;
    let (remainder, (seeds, _, _)) = tuple((
        separated_list1(tag(" "), map_res(digit1, i64::from_str)),
        line_ending,
        line_ending,
    ))(remainder)?;
    Ok((remainder, seeds))
}

fn part_1(contents: &str) {
    let (remaining, seeds) = parse_seeds_part1(&contents).unwrap();
    let (_, transforms) = parse_transforms(&remaining).unwrap();
    let mut locations = vec![];
    for seed in &seeds {
        let mut x = *seed;

        for transform in &transforms {
            let affine_transform = gen_affine_transform(transform);
            x = affine_transform(x);
        }
        locations.push(x);
    }
    locations.sort();
    println!("part 1: {:?}", locations)
}

fn part_2(contents: &str) {
    let (remaining, seeds) = parse_seeds_part1(&contents).unwrap();
    let (_, transforms) = parse_transforms(&remaining).unwrap();
    //let mut stage = vec![];

    // seeds now represent (start, n), partition up into tuples
    let mut ranges: Vec<(i64, i64)> = seeds.chunks(2).map(|x| (x[0], x[0] + x[1] - 1)).collect();

    //     for (x,y) in &ranges {
    //         tmp.push(*x);
    //         tmp.push(*y);
    //    }

    //    tmp.sort();
    //    ranges = tmp.chunks(2).map(|x| (x[0], x[1])).collect();
    //    tmp = vec![];

    //    println!("part 2: {:?}", ranges);

    // for (x, y) in ranges {
    //     let iter = x..y;
    //     for i in iter {
    //         stage.push(i);
    //     }
    // }
    for transform in &transforms {
        let mut stages = vec![];
        println!("step");
        let split_affines = gen_split_affine_transform(transform);

        for (x, y) in &ranges {
            let tmp = split_affines(*x, *y);
            println!("{:?}", tmp);

            for (a, b) in tmp {
                stages.push((a, b));
            }
        }
        println!("there are {} stages", stages.len());

        ranges = stages;
    }
    //println!("{:?}", split_affines(a,b));

    // for transform in &transforms {
    //     println!("step");
    //     let affine_transform = gen_affine_transform(transform);

    //     // now taking the current range, find where
    //     // the affine transform is the identity and
    //     // create a new range

    //     stage = stage.iter().map(|x| affine_transform(*x)).collect();
    //     //tmp.sort();
    //     //ranges = tmp.chunks(2).map(|x| (x[0], x[1])).collect();
    //     //res = tmp;
    // }
    // stage.sort();
    // println!("part 2: {}", stage[0] )
}

fn main() {
    let contents = fs::read_to_string("../data/5.txt").unwrap();
    part_1(&contents);
    part_2(&contents);
}
