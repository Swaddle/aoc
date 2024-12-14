use nom::{
    IResult,
    bytes::complete::tag,
    character::complete::{digit1, line_ending},
    combinator::{map_res, opt},
    multi::many0,
    multi::separated_list0,
    sequence::tuple,
};

type Claw = (i64, i64, i64, i64, i64, i64);

pub fn parse_i64(input: &str) -> IResult<&str, i64> {
    map_res(digit1, str::parse::<i64>)(input)
}

pub fn parse_a_row(input: &str) -> IResult<&str, (i64, i64)> {
    let (remaining, (_, a, _, b, _)) = tuple((
        tag("Button A: X+"),
        parse_i64,
        tag(", Y+"),
        parse_i64,
        line_ending,
    ))(input)?;

    Ok((remaining, (a, b)))
}

pub fn parse_b_row(input: &str) -> IResult<&str, (i64, i64)> {
    let (remaining, (_, a, _, b, _)) = tuple((
        tag("Button B: X+"),
        parse_i64,
        tag(", Y+"),
        parse_i64,
        line_ending,
    ))(input)?;

    Ok((remaining, (a, b)))
}

pub fn parse_prize_row(input: &str) -> IResult<&str, (i64, i64)> {
    let (remaining, (_, a, _, b, _)) = tuple((
        tag("Prize: X="),
        parse_i64,
        tag(", Y="),
        parse_i64,
        line_ending,
    ))(input)?;

    Ok((remaining, (a, b)))
}

pub fn parse_block(input: &str) -> IResult<&str, Claw> {
    let (remaining, ((a, b), (c, d), (p1, p2))) =
        tuple((parse_a_row, parse_b_row, parse_prize_row))(input)?;

    Ok((
        remaining,
        (a, b, c, d, 10000000000000 + p1, 10000000000000 + p2),
    ))
}

pub fn parse_input(input: &str) -> IResult<&str, Vec<Claw>> {
    let (rem, blocks) = separated_list0(tag("\n"), parse_block)(input)?;
    Ok((rem, blocks))
}

pub fn solve_block(block: Claw) -> Option<(i64, i64)> {
    let (x1, y1, x2, y2, p1, p2) = block;

    let denom = (x1 * y2 - x2 * y1);
    if denom == 0 {
        return None;
    }
    let x = (y2 * p1 - x2 * p2);
    let y = (x1 * p2 - y1 * p1);

    let xrem = x % denom;
    let yrem = y % denom;

    let x0 = (x / denom);
    let y0 = (y / denom);

    // check if remainder i

    if x0 >= 0 && y0 >= 0 && xrem == 0 && yrem == 0 {
        return Some((x0, y0));
    }
    None
}

pub fn p1() -> u64 {
    let input = std::fs::read_to_string("../data/13.txt.fen").unwrap();
    let (rem, blocks) = parse_input(&input).unwrap();
    let sols = blocks
        .iter()
        .map(|block| solve_block(*block))
        .collect::<Vec<Option<(i64, i64)>>>();

    println!("{:?}", sols);

    // sum it all up
    let sol = sols
        .iter()
        .map(|p| match p {
            Some((x, y)) => 3 * x + y,
            None => 0,
        })
        .sum::<i64>();

    println!("{:?}", sol);
    return 0;
}
