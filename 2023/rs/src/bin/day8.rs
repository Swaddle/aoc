use nom::{
    branch::alt, bytes::complete::tag, bytes::complete::take_while,
    character::complete::line_ending, multi::many1, sequence::tuple, IResult,
};

use std::fs;

fn parse_instructions(input: &str) -> IResult<&str, Vec<&str>> {
    Ok(many1(alt((tag("L"), tag("R"))))(input)?)
}

fn parse_node(input: &str) -> IResult<&str, (&str, (&str, &str))> {
    let (remaining, (key, _, _, _, _, left, _,_, right, _, _)) = tuple((
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

// takes the map, a key, and an instruction, returns the next key
// fn step(node_map: &HashMap<&str, (&str, &str)>, key: &str, instruction: &str) -> Option<&str> {
//     match instruction {
//         "L" => node_map.get(key).map(|(left, _)| *left),
//         "R" => node_map.get(key).map(|(_, right)| *right),
//         _ => None,
//     }
// }

fn main() {
    let contents = fs::read_to_string("../data/8.txt").unwrap();
    let (_remaining, instructions) = parse_instructions(&contents).unwrap();
    let _instrunction_cycle = instructions.iter().cycle();
}
