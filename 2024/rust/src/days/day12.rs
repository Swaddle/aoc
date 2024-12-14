use std::collections::HashMap;

#[derive(Debug)]
pub struct Plot {
    cells: Vec<(i32, i32)>,
    edges: Vec<(i32, i32)>,
    area: i32,
}

pub fn neighbours(
    (x, y): (i32, i32),
    key: &char,
    grid: &HashMap<(i32, i32), char>,
) -> (bool, Vec<(i32, i32)>) {
    // check left/right/above/below
    let dirs: Vec<(i32, i32)> = vec![(0, 1), (0, -1), (1, 0), (-1, 0)];
    let neighbours: Vec<(i32, i32)> = dirs
        .iter()
        .map(|(dx, dy)| (x + dx, y + dy))
        .filter_map(|pos| (grid.get(&pos) == Some(&key)).then_some(pos))
        .collect();
    // its a border if its not surrounded by 4 other cells
    return (neighbours.len() < 4, neighbours);
}

pub fn p1() -> u64 {
    let input = std::fs::read_to_string("../data/12.txt").unwrap();
    let lines = input.lines();

    // parse into hashmap of
    // (x, y) -> 'character'
    let mut grid: HashMap<(i32, i32), char> = HashMap::new();
    for (y, line) in lines.enumerate() {
        for (x, c) in line.chars().enumerate() {
            grid.insert((x as i32, y as i32), c);
        }
    }

    println!("{:?}", grid);

    // parse the input into a list of plots
    let plots: Vec<Plot> = Vec::new();

    // iterate over the grid,
    for ((x, y), &c) in &grid {
        let (is_border, neighbours) = neighbours((*x, *y), &c, &grid);
        if is_border {
            println!("border: {:?}", (x, y));
        }
    }
    // for each row in the input
    // for example
    // ACCCAABBB
    // look at neighbours to the left, right and above

    return 0;
}
