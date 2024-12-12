use fxhash::FxHashMap as HashMap;

#[inline(always)]
pub fn f(x: usize) -> (usize, Option<usize>) {
    match x {
        0 => (1, None),
        x if x.ilog10() % 2 == 1 => {
            let div = 10usize.pow(x.ilog10() / 2 + 1);
            (x / div, Some(x % div))
        }
        _ => (x * 2024, None),
    }
}

pub fn solve(input: Vec<usize>, blinks: usize) -> usize {
    let mut counts: HashMap<usize, usize> = input.iter().cloned().map(|x| (x, 1)).collect();
    let mut new_counts: HashMap<usize, usize> = HashMap::default();

    for _ in 0..blinks {
        for (key, val) in counts.iter() {
            let (a, b) = f(*key as usize);
            if let Some(b) = b {
                *new_counts.entry(b).or_insert(0) += *val;
            }
            *new_counts.entry(a).or_insert(0) += *val;
        }
        std::mem::swap(&mut counts, &mut new_counts);
        new_counts.clear();
    }
    counts.values().sum()
}

pub fn p1() -> usize {
    let t0 = std::time::Instant::now();
    let input = std::fs::read_to_string("../data/11.txt").unwrap();

    let t1 = t0.elapsed();
    println!("read: {:?}", t1);

    let input = input
        .trim()
        .split_whitespace()
        .map(|x| x.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();

    let total = solve(input, 75);

    let t2 = t0.elapsed();
    println!("total: {:?}", total);
    println!("elapsed: {:?}", t2);
    return 0;
}
