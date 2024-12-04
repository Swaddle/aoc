//use memmap2::Mmap;

pub fn get_input(filename: &str) -> String {
    //let file = std::fs::File::open(filename).unwrap();

    let contents = std::fs::read_to_string(filename).unwrap();

    //let mmap = unsafe { Mmap::map(&file).unwrap() };
    //let input = String::from_utf8_lossy(&mmap).to_string();
    //return input;
    return contents;
}

pub fn parse_line(line: &str) -> (i32, i32) {
    let mut parts = line.split_whitespace();
    let a = parts.next().unwrap().parse::<i32>().unwrap();
    let b = parts.next().unwrap().parse::<i32>().unwrap();
    return (a, b);
}

pub fn parse_input(input: &str) -> (Vec<i32>, Vec<i32>) {
    return input.lines().map(|line| parse_line(line)).unzip();
}

pub fn p1() -> u64 {
    //println!("get_input: {}", get_input("../data/01.txt"));

    let t_0 = std::time::Instant::now();

    let input = get_input("../data/01.txt");

    let (mut v1, mut v2) = parse_input(&input);

    v1.sort_unstable();
    v2.sort_unstable();

    // sum abs v1 - v2 as iterator
    let sum: u32 = v1.iter().zip(v2).map(|(a, b)| a.abs_diff(b)).sum();

    let t_1 = t_0.elapsed();
    println!("time: {:?}", t_1);

    println!("part 1: {}", sum);

    return sum as u64;
}

pub fn p2() -> u64 {
    let t_0 = std::time::Instant::now();

    let input = get_input("../data/01.txt");
    let (v1, v2) = parse_input(&input);

    let mut v2_map = std::collections::HashMap::new();

    v2.iter().for_each(|&x| {
        let count = v2_map.entry(x).or_insert(0);
        *count += 1;
    });

    // for each entry
    // count weighted by count in map or 0
    let sum: i32 = v1.iter().map(|x| x * v2_map.get(x).unwrap_or(&0)).sum();

    let t_1 = t_0.elapsed();
    println!("time: {:?}", t_1);

    println!("part 2: {:?}", sum);

    return sum as u64;
}
