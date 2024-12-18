use std::collections::HashMap;
use std::thread::sleep;
use std::time::Duration;

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

type Pt2d = (i32, i32);

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

pub fn render_p1(walls: &Vec<Wall>, boxes: &mut Vec<Box>, robot: &mut Robot, moves: &Vec<Pt2d>) {
    
    let grid_height = walls.iter().map(|Wall { y, .. }| y).max().unwrap() + 1;
    let grid_width = walls.iter().map(|Wall { x, .. }| x).max().unwrap() + 1;

    for (dx, dy) in moves {
        let mut grid = vec![vec!['.'; grid_width as usize]; grid_height as usize];

        // take the robots current position 
        // grab the slice of boxes in that direction
        let position = (robot.x, robot.y);
        let boxes_to_move = slice(walls, boxes, position, (*dx, *dy));

        match boxes_to_move {
            Some(mut boxes_to_move) => {
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
            },
            None => {
                // dont move the robot
            }
        }
        println!("\x1B[2J\x1B[H");

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
                print!("{}", cell);
            }
            println!();
        }
        sleep(Duration::from_millis(100));

        //
    }

    // sum up all the box coords at the end 
    let sum = boxes.iter().map(|Box { x, y }| x + 100*y).sum::<i32>();
    println!("sum: {}", sum);
}

pub fn p1() -> u64 {
    let input = std::fs::read_to_string("../data/15.txt.fen").unwrap();
    let (walls, mut boxes, moves, mut robot) = parse_input_p1(&input);

    render_p1(&walls,  &mut boxes, &mut robot, &moves);

    return 0;
}
