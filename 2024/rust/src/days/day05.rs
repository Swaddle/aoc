use nom::{
    IResult,
    bytes::complete::tag,
    character::complete::{digit1, line_ending},
    combinator::{map_res, opt},
    multi::{many0, separated_list0},
    sequence::tuple,
};

fn parse_u32(input: &str) -> IResult<&str, u32> {
    map_res(digit1, str::parse::<u32>)(input)
}

pub fn parse_rules_row(input: &str) -> IResult<&str, (u32, u32)> {
    let (remaining, (a, _, b, _)) = tuple((parse_u32, tag("|"), parse_u32, line_ending))(input)?;
    Ok((remaining, (a, b)))
}

pub fn parse_page_row(input: &str) -> IResult<&str, Vec<u32>> {
    let (remaining, (row, _)) = tuple((separated_list0(tag(","), parse_u32), line_ending))(input)?;
    Ok((remaining, row))
}

pub fn parse(input: &str) -> IResult<&str, (Vec<(u32, u32)>, Vec<Vec<u32>>)> {
    let (remaining, rules) = many0(parse_rules_row)(input)?;
    let (remaining, _) = opt(line_ending)(remaining)?;
    let (remaining, pages) = many0(parse_page_row)(&remaining)?;
    Ok((remaining, (rules, pages)))
}

pub fn p1() -> u64 {
    let t0 = std::time::Instant::now();
    let input = std::fs::read_to_string("../data/05.txt").unwrap();
    let (_rem, (_rules, _pages)) = parse(&input).unwrap();
    let t1 = t0.elapsed();

    println!("time: {:?}", t1);
    //println!("{:?}", rules);
    return 0;
}
