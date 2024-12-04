pub fn get_input(filename: &str) -> String {
    let contents = std::fs::read_to_string(filename).unwrap();
    return contents;
}

pub fn check_p1(grid: &std::collections::HashMap<(i32, i32), char>, x: i32, y: i32) -> u32 {
    let target = ['M', 'A', 'S'];
    let directions = [
        (1, 0),
        (-1, 0),
        (0, 1),
        (0, -1),
        (1, 1),
        (-1, 1),
        (1, -1),
        (-1, -1),
    ];

    let path = |dx: i32, dy: i32| -> Vec<Option<char>> {
        (1..=3)
            .map(|i| grid.get(&(x + i * dx, y + i * dy)).copied())
            .collect()
    };

    let mut count = 0;

    if grid.get(&(x, y)).copied() == Some('X') {
        for &(dx, dy) in &directions {
            let slice = path(dx, dy);
            if slice == target.iter().map(|&c| Some(c)).collect::<Vec<_>>() {
                count += 1;
            }
        }
    }

    return count;
}

pub fn p1() -> u64 {
    //input is a grid
    //store grid as hashmap of (x, y)-> char

    let input = get_input("../data/04.txt");
    let mut grid = std::collections::HashMap::new();

    let rows = input.lines().enumerate();
    for (y, row) in rows {
        let cols = row.chars().enumerate();
        for (x, c) in cols {
            grid.insert((x as i32, y as i32), c);
        }
    }

    let sum: u32 = grid.iter().map(|(k, _)| check_p1(&grid, k.0, k.1)).sum();

    println!("part 1: {}", sum);
    //for each cell in grid
    //check if starts with 'X'
    //and then check all 8 directions
    //if any direction has 'MAS' then increment count

    return 0;
}

pub fn p2() -> u64 {
    return 0;
}
