pub fn ways(target: &str, patterns: &Vec<&str>) -> u64 {
    let n = target.len();
    let mut dp = vec![0; n + 1];
    dp[0] = 1;

    (1..=n).for_each(|k| {
        patterns.iter().for_each(|pattern| {
            if k >= pattern.len() && target[k - pattern.len()..k] == **pattern {
                dp[k] += dp[k - pattern.len()];
            }
        });
    });

    return dp[target.len()];
}

pub fn p1() -> u64 {
    let t0 = std::time::Instant::now();
    let input = std::fs::read_to_string("../data/19.txt").unwrap();
    let input = input.split("\n\n").collect::<Vec<&str>>();
    // input is patterns \n\n towels 
    let patterns = input[0].split(", ").collect::<Vec<&str>>();
    let towel = input[1].lines().collect::<Vec<&str>>();

    let mut count = 0;
    let mut total_ways = 0;

    for t in &towel {
        let ways = ways(t, &patterns);
        if ways > 0 {
            count += 1;
        }
        total_ways += ways;
    }
    let elapsed = t0.elapsed();
    println!("elapsed: {:?}", elapsed);
    println!("{:?}", count);
    println!("{:?}", total_ways);
    return 0
}