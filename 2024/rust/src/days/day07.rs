use nom::{
    IResult,
    bytes::complete::tag,
    character::complete::{digit1, line_ending},
    combinator::map_res,
    multi::{many0, separated_list0},
    sequence::tuple,
};

pub fn parse_u64(input: &str) -> IResult<&str, u64> {
    map_res(digit1, str::parse::<u64>)(input)
}

pub fn parse_row(input: &str) -> IResult<&str, (u64, Vec<u64>)> {
    let (remaining, (target, _, values, _)) = tuple((
        parse_u64,
        tag(": "),
        separated_list0(tag(" "), parse_u64),
        line_ending,
    ))(input)?;
    println!("{:?}", values);
    Ok((remaining, (target, values)))
}

pub fn parse_input(input: &str) -> IResult<&str, Vec<(u64, Vec<u64>)>> {
    // input is a row of a target: and possible values to make up target
    // from + and *
    // target: x1 x2 x3 etc
    many0(parse_row)(input)
}

// find the combination of + and * evaluated left to right that equals target
// returns Some(values) or None
// must use all the numbers
pub fn possible(target: u64, values: Vec<u64> ) -> Option<Vec<u64>> {
    
    // work backwards from the target 
    let ops = Vec::new();

    let mul = |a: u64, b: u64| a * b;
    let add = |a: u64, b: u64| a + b;

    let mut x = target;

    // iterate through the values backwards 
    for i in (0..values.len()).rev() {
        let v = values[i];


        // if target is divisible by the value, we know the op 
        // must be multiplication
        if x % v == 0 {
            ops.push(mul);
            x = x / v;
            if x == 1 {
                return Some(values);
            }
        } else {
            ops.push(add);
            x = x - v;

            if x == 0 {
                return Some(values);
            }
    
        }

    }    
}

pub fn p1() {
    let t0 = std::time::Instant::now();
    let input = std::fs::read_to_string("../data/07.txt").unwrap();
    let (rem, input) = parse_input(&input).unwrap();
    let t1 = t0.elapsed();



    println!("time: {:?}", t1);
    println!("{:?}", input);
}
