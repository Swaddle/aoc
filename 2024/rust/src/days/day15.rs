use std::collections::HashMap;
use std::thread::sleep;
use std::time::Duration;
use std::collections::{HashSet, VecDeque};

#[derive(PartialEq, Eq, Debug)]
pub struct Robot {
    pub x: i32,
    pub y: i32,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Box {
    pub x: i32,
    pub y: i32,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Wall {
    pub x: i32,
    pub y: i32,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct BigBox {
    pub x0: i32,
    pub x1: i32,
    pub y0: i32,
}

type Pt2d = (i32, i32);

pub fn parse_input_p1(input: &str) -> (Vec<Wall>, Vec<Box>, Vec<Pt2d>, Robot) {
    let mut walls = Vec::new();
    let mut boxes = Vec::new();
    let mut moves = Vec::new();
    let mut robot = Robot { x: 0, y: 0 };

    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            match c {
                '#' => walls.push(Wall {
                    x: x as i32,
                    y: y as i32,
                }),
                'O' => boxes.push(Box {
                    x: x as i32,
                    y: y as i32,
                }),
                '@' => {
                    robot = Robot {
                        x: x as i32,
                        y: y as i32,
                    }
                }
                '^' => moves.push((0, -1)),
                'v' => moves.push((0, 1)),
                '<' => moves.push((-1, 0)),
                '>' => moves.push((1, 0)),
                _ => (),
            }
        }
    }
    return (walls, boxes, moves, robot);
}

pub fn parse_input_p2(input: &str) -> (Vec<Wall>, Vec<BigBox>, Vec<Pt2d>, Robot) {
    let mut walls = Vec::new();
    let mut boxes = Vec::new();
    let mut moves = Vec::new();
    let mut robot = Robot { x: 0, y: 0 };

    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            match c {
                '#' => {
                    walls.push(Wall {
                        x: 2 * x as i32,
                        y: y as i32,
                    });
                    walls.push(Wall {
                        x: ((2 * x) + 1) as i32,
                        y: y as i32,
                    });
                }
                'O' => boxes.push(BigBox {
                    x0: 2 * x as i32,
                    x1: ((2 * x) + 1) as i32,
                    y0: y as i32,
                }),
                '@' => {
                    robot = Robot {
                        x: 2 * x as i32,
                        y: y as i32,
                    }
                }
                '^' => moves.push((0, -1)),
                'v' => moves.push((0, 1)),
                '<' => moves.push((-1, 0)),
                '>' => moves.push((1, 0)),
                _ => (),
            }
        }
    }
    return (walls, boxes, moves, robot);
}

pub fn slice(
    walls: &Vec<Wall>,
    boxes: &Vec<Box>,
    start: (i32, i32),
    direction: (i32, i32),
) -> Option<Vec<Box>> {
    // given a start position
    // and a direction
    // return a list of movable boxes that are in that direction
    // until a wall or free space is encountered
    let (mut x, mut y) = start;
    let (dx, dy) = direction;
    let mut slice = Vec::new();

    loop {
        x += dx;
        y += dy;
        if walls.contains(&Wall { x, y }) {
            return None;
        }
        if boxes.contains(&Box { x, y }) {
            slice.push(Box { x, y });
        } else {
            return Some(slice);
        }
    }
}

pub fn solve_p1(
    walls: &Vec<Wall>,
    boxes: &mut Vec<Box>,
    robot: &mut Robot,
    moves: &Vec<Pt2d>,
    render: bool,
) {
    let grid_height = walls.iter().map(|Wall { y, .. }| y).max().unwrap() + 1;
    let grid_width = walls.iter().map(|Wall { x, .. }| x).max().unwrap() + 1;

    for (dx, dy) in moves {
        // take the robots current position
        // grab the slice of boxes in that direction
        let position = (robot.x, robot.y);
        let boxes_to_move = slice(walls, boxes, position, (*dx, *dy));

        match boxes_to_move {
            Some(boxes_to_move) => {
                // move the robot
                robot.x += dx;
                robot.y += dy;

                // find the boxes in the slice in boxes
                // and move them
                for bx in boxes_to_move.iter() {
                    let index = boxes.iter().position(|b| b == bx).unwrap();
                    boxes[index].x += dx;
                    boxes[index].y += dy;
                }
            }
            None => {
                // dont move the robot
            }
        }

        if render {
            println!("\x1B[2J\x1B[H");
            let mut grid = vec![vec!['.'; grid_width as usize]; grid_height as usize];

            // render the grid
            for Wall { x, y } in walls.iter() {
                grid[*y as usize][*x as usize] = '#';
            }

            for Box { x, y } in boxes.iter() {
                grid[*y as usize][*x as usize] = 'O';
            }

            grid[robot.y as usize][robot.x as usize] = '@';

            for row in grid.iter() {
                for cell in row.iter() {
                    print!("c{}", cell);
                }
                println!();
            }

            sleep(Duration::from_millis(100));
        }

        //
    }

    // sum up all the box coords at the end
    let sum = boxes.iter().map(|Box { x, y }| x + 100 * y).sum::<i32>();
    println!("sum: {}", sum);
}

pub fn slice_2(
    walls: &Vec<Wall>,
    boxes: &Vec<BigBox>,
    start: (i32, i32),
    direction: (i32, i32),
) -> Option<Vec<BigBox>> {
    let (x, y) = start;
    let (dx, dy) = direction;
    let mut queue = VecDeque::from([(x, y)]);
    let mut seen = HashSet::new();

    while let Some((cx, cy)) = queue.pop_front() {
        if !seen.insert((cx, cy)) {
            continue;
        }

        let (nx, ny) = (cx + dx, cy + dy);

        // Check for walls
        if walls.contains(&Wall { x: nx, y: ny }) {
            return None;
        }

        if let Some(current_box) = boxes.iter().find(|b| {
            (b.x0 == nx && b.x1 == nx + 1 && b.y0 == ny) || (b.x1 == nx && b.x0 == nx - 1 && b.y0 == ny)
        }) {
            queue.extend([(current_box.x0, current_box.y0), (current_box.x1, current_box.y0)]);
        }
    }

    let mut collected_boxes: Vec<BigBox> = seen
        .iter()
        .filter_map(|&(sx, sy)| {
            boxes.iter().find(|b| b.x0 == sx && b.y0 == sy).copied()
        })
        .collect();

    collected_boxes.sort_by_key(|b| (x.abs_diff(b.x0), y.abs_diff(b.y0)));

    for b in &collected_boxes {
        let (nx0, nx1, ny) = (b.x0 + dx, b.x1 + dx, b.y0 + dy);

        if walls.contains(&Wall { x: nx0, y: ny }) || walls.contains(&Wall { x: nx1, y: ny }) {
            return None;
        }

        if boxes.iter().any(|other| {
            (other.x0 == nx0 && other.y0 == ny) || (other.x1 == nx1 && other.y0 == ny)
        }) {
            return None;
        }
    }

    Some(collected_boxes)
}

pub fn solve_p2(
    walls: &Vec<Wall>,
    boxes: &mut Vec<BigBox>,
    robot: &mut Robot,
    moves: &Vec<Pt2d>,
    render: bool,
) {
    let grid_height = walls.iter().map(|Wall { y, .. }| y).max().unwrap() + 1;
    let grid_width = walls.iter().map(|Wall { x, .. }| x).max().unwrap() + 1;

    for (dx, dy) in moves {
        // take the robots current position
        // grab the slice of boxes in that direction
        let position = (robot.x, robot.y);
        let boxes_to_move = slice_2(walls, boxes, position, (*dx, *dy));

        println!("{:?}", boxes_to_move);

        match boxes_to_move {
            Some(ref boxes_to_move) => {
                // move the robot
                robot.x += dx;
                robot.y += dy;

                // find the boxes in the slice in boxes
                // and move them
                for bx in boxes_to_move.iter() {
                    let index = boxes.iter().position(|b| b == bx).unwrap();
                    boxes[index].x0 += dx;
                    boxes[index].x1 += dx;
                    boxes[index].y0 += dy;
                }
            }
            None => {
                // dont move the robot
            }
        }

        if render {
            println!("\x1B[2J\x1B[H");
            let mut grid = vec![vec!['.'; grid_width as usize]; grid_height as usize];

            // render the grid
            for Wall { x, y } in walls.iter() {
                grid[*y as usize][*x as usize] = '#';
            }

            for BigBox { x0, x1, y0 } in boxes.iter() {
                // left side of box is [ character
                // right side of box is ] character
                grid[*y0 as usize][*x0 as usize] = '[';
                grid[*y0 as usize][*x1 as usize] = ']';
            }

            grid[robot.y as usize][robot.x as usize] = '@';

            for row in grid.iter() {
                for cell in row.iter() {
                    print!("{}", cell);
                }
                println!();
            }

            let display_move = match (dx, dy) {
                (0, -1) => "^",
                (0, 1) => "v",
                (-1, 0) => "<",
                (1, 0) => ">",
                _ => " ",
            };

            println!("move: {}", display_move);
            println!("boxes to move: {:?}", boxes_to_move.unwrap_or(vec![]));

            sleep(Duration::from_millis(500));
        }

        //
    }
}
// sum up all

pub fn p1() -> u64 {
    let t0 = std::time::Instant::now();
    let input = std::fs::read_to_string("../data/15.txt.fen").unwrap();
    let (walls, mut boxes, moves, mut robot) = parse_input_p1(&input);
    solve_p1(&walls, &mut boxes, &mut robot, &moves, false);
    let t1 = t0.elapsed();
    println!("elapsed: {:?}", t1);
    return 0;
}

pub fn p2() -> u64 {
    let t0 = std::time::Instant::now();
    let input = std::fs::read_to_string("../data/15.txt").unwrap();
    let (walls, mut boxes, moves, mut robot) = parse_input_p2(&input);
    solve_p2(&walls, &mut boxes, &mut robot, &moves, true);
    let t1 = t0.elapsed();
    println!("elapsed: {:?}", t1);
    return 0;
}
