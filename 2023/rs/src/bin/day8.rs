use nom::{
    branch::alt, bytes::complete::tag, bytes::complete::take_while,
    character::complete::line_ending, multi::many1, sequence::tuple, IResult,
};
use std::collections::HashMap;
use std::fs;
use num::Integer;

fn parse_instructions(input: &str) -> IResult<&str, Vec<&str>> {
    Ok(many1(alt((tag("L"), tag("R"))))(input)?)
}

fn parse_node(input: &str) -> IResult<&str, (&str, (&str, &str))> {
    let (remaining, (key, _, _, _, _, left, _, _, right, _, _)) = tuple((
        take_while(char::is_alphanumeric),
        tag(" "),
        tag("="),
        tag(" "),
        tag("("),
        take_while(char::is_alphanumeric),
        tag(","),
        tag(" "),
        take_while(char::is_alphanumeric),
        tag(")"),
        line_ending,
    ))(input)?;
    Ok((remaining, (key, (left, right))))
}

fn parse_nodes(input: &str) -> IResult<&str, Vec<(&str, (&str, &str))>> {
    let (remaining, (_, _, vec)) = tuple((line_ending, line_ending, many1(parse_node)))(input)?;
    Ok((remaining, vec))
}

//takes the map, a key, and an instruction, returns the next key
fn step<'a>(
    node_map: &'a HashMap<&'a str, (&'a str, &'a str)>,
    key: &'a str,
    instruction: &'a str,
) -> Option<&'a str> {
    match instruction {
        "L" => node_map.get(key).map(|(left, _)| *left),
        "R" => node_map.get(key).map(|(_, right)| *right),
        _ => None,
    }
}

fn calc_path_length(start: &str, end_check: impl Fn(&str) -> bool, instructions: &Vec<&str>, node_map: &HashMap<&str, (&str, &str)>) -> u64 {
    let mut count = 0;
    let mut key = start;
    let mut _instrunction_cycle = instructions.iter().cycle();
    while end_check(key) {
        let Some(new_key) = step(&node_map, key, _instrunction_cycle.next().unwrap()) else {
            break;
        };
        count += 1;
        key = new_key;
    }
    println!("ended on: {}, in {} steps", key, count);
    count 
}

fn find_end<'a>(end: &str, nodes: &'a Vec<(&'a str, (&'a str, &'a str))>) -> Vec<&'a str> {
    nodes
        .iter()
        .filter(|(key, (_, _))| key.ends_with(end) )
        .map(|(key, (_, _))| *key)
        .collect()
}


fn main() {
    let contents = fs::read_to_string("../data/8.txt").unwrap();
    let (remaining, instructions) = parse_instructions(&contents).unwrap();
    let (_, nodes) = parse_nodes(&remaining).unwrap();
    let node_map: HashMap<&str, (&str, &str)> = nodes.clone().into_iter().collect();
    
    let end_check = move |key: &str| key != "ZZZ";
    let part_1 = calc_path_length("AAA", end_check, &instructions, &node_map);
    println!("part 1: {}", part_1);

    let starts = find_end("A",&nodes).into_iter();

    println!("{:?}", starts.clone().collect::<Vec<&str>>());
    
    let end_check = move |key: &str| !key.ends_with("Z");

    let lengths = starts.map(|start| calc_path_length(start, end_check, &instructions, &node_map)).collect::<Vec<u64>>();
    let lcm = lengths.iter().fold(1, |acc: u64, x| acc.lcm(x));

    println!("part 2: {}", lcm);
    //let lengths: Vec<u64> = boundaries.map(|(start, end)| calc_path_length(start, end, &instructions, &node_map)).collect();

    println!("{:?}",lengths);
}
