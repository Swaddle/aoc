pub fn get_input(filename: &str) -> String {
    let contents = std::fs::read_to_string(filename).unwrap();
    return contents;
}

pub fn check_p1(grid: &std::collections::HashMap<(i32, i32), char>, x: i32, y: i32) -> u32 {
    let target = [Some('M'), Some('A'), Some('S')];
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
            if slice == target {
                count += 1;
            }
        }
    }

    return count;
}

pub fn check_p2(grid: &std::collections::HashMap<(i32, i32), char>, x: i32, y: i32) -> u32 {
    // check if the cell is an 'A'

    // if it is check the diagonals are an 'M' and 'S'

    let target = [Some('M'), Some('S')];
    let reverse_target = [Some('S'), Some('M')];

    // any combination of
    //  M .. S
    //  .. A ..
    //  S .. M
    let mut count = 0;
    if grid.get(&(x, y)).copied() == Some('A') {
        //
        let diag_1 = [
            grid.get(&(x + 1, y + 1)).copied(),
            grid.get(&(x - 1, y - 1)).copied(),
        ];
        let diag_2 = [
            grid.get(&(x + 1, y - 1)).copied(),
            grid.get(&(x - 1, y + 1)).copied(),
        ];
        if (diag_1 == target || diag_1 == reverse_target)
            && (diag_2 == target || diag_2 == reverse_target)
        {
            count += 1;
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
    return sum as u64;
}

pub fn p2() -> u64 {
    let input = get_input("../data/04.txt");
    let mut grid = std::collections::HashMap::new();

    let rows = input.lines().enumerate();
    for (y, row) in rows {
        let cols = row.chars().enumerate();
        for (x, c) in cols {
            grid.insert((x as i32, y as i32), c);
        }
    }

    let sum: u32 = grid.iter().map(|(k, _)| check_p2(&grid, k.0, k.1)).sum();

    println!("part 2: {}", sum);

    return sum as u64;
}
