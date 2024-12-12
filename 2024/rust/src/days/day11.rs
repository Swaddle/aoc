
use fxhash::FxHashMap as HashMap;

#[inline(always)]
pub fn f(x: u64) -> (u64, Option<u64>) {
    if x == 0 {
        return (1, None);
    }
    let mut digits = 0;
    let mut temp = x;
    while temp > 0 {
        digits += 1;
        temp /= 10;
    }
    if digits % 2 == 0 { 
        let half = digits / 2;
        let div = 10_u64.pow(half);
        let a = x / div;
        let b = x % div;
        return (a, Some(b));
    }
    return (x * 2024, None);
}


pub fn solve(input: Vec<u64>, blinks: u32) -> u64 {
    let mut counts: HashMap<u64, u64> = HashMap::default();
    for x in input {
        *counts.entry(x).or_insert(0) += 1;
    }
    let mut new_counts: HashMap<u64, u64> = HashMap::default();
    for _ in 0..blinks {
        new_counts.clear();
        for (key, val) in counts.iter() {
            let (a,b) = f(*key);
            *new_counts.entry(a).or_insert(0) += *val;
            if let Some(b) = b {
                *new_counts.entry(b).or_insert(0) += *val;
            }
        }
        std::mem::swap(&mut counts, &mut new_counts);
    }
    counts.values().sum()
}


pub fn p1() -> u64 {
    let t0 = std::time::Instant::now();
    let input = std::fs::read_to_string("../data/11.txt").unwrap();
    let input = input.trim().split_whitespace().map(|x| x.parse::<u64>().unwrap()).collect::<Vec<u64>>();
    let total = solve(input, 75);
    println!("total: {:?}", total);
    let t1 = t0.elapsed();
    println!("elapsed: {:?}", t1);
    return 0;
}
