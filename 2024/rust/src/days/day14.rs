use rayon::prelude::*;
use std::thread;

#[derive(Debug, Clone)]
struct Robot {
    x: i32,
    y: i32,
    v_x: i32,
    v_y: i32,
}

pub fn parse_input(input: &str) -> Vec<Robot> {
    let mut robots = Vec::new();
    for line in input.lines() {
        let parts = line.split(" ").collect::<Vec<&str>>();
        let p = parts[0].split("=").collect::<Vec<&str>>()[1];
        let v = parts[1].split("=").collect::<Vec<&str>>()[1];
        let p = p.split(",").collect::<Vec<&str>>();
        let v = v.split(",").collect::<Vec<&str>>();
        let robot = Robot {
            x: p[0].parse::<i32>().unwrap(),
            y: p[1].parse::<i32>().unwrap(),
            v_x: v[0].parse::<i32>().unwrap(),
            v_y: v[1].parse::<i32>().unwrap(),
        };
        robots.push(robot);
    }
    return robots;
}

pub fn print_grid(
    robots: &mut Vec<Robot>,
    grid_width: usize,
    grid_height: usize,
    simulation_time: usize,
) {
    let mut grid = vec![vec![0; grid_width]; grid_height];

    for mut robot in robots.iter_mut() {
        simulate_robot(
            &mut robot,
            grid_width as i32,
            grid_height as i32,
            simulation_time as i32,
        );
    }

    for Robot { x, y, .. } in robots.iter_mut() {
        grid[*y as usize][*x as usize] += 1;
    }
    for row in &grid {
        for &cell in row {
            if cell > 0 {
                print!("{}", cell); // Print number of robots in the cell
            } else {
                print!("."); // Empty cell
            }
        }
        println!();
    }
}

fn animate_simulation(
    robots: &mut Vec<Robot>,
    grid_width: usize,
    grid_height: usize,
    simulation_time: usize,
) {
    let sleep_duration = std::time::Duration::from_millis(500);
    for t in 0..simulation_time {
        let mut grid = vec![vec![0; grid_width]; grid_height];
        for mut robot in robots.iter_mut() {
            simulate_robot(&mut robot, grid_width as i32, grid_height as i32, 1);
        }
        for Robot { x, y, .. } in robots.iter_mut() {
            grid[*y as usize][*x as usize] += 1;
        }
        println!("\x1B[2J\x1B[H");
        for row in &grid {
            for &cell in row {
                if cell > 0 {
                    print!("{}", cell); // Print number of robots in the cell
                } else {
                    print!("."); // Empty cell
                }
            }
            println!();
        }
        println!("\n step {} ", t);
        thread::sleep(sleep_duration);
    }
}

fn update_step(a: &mut i32, old_a: &mut i32, quotient: i32) {
    let temp = *a;
    *a = *old_a - quotient * temp;
    *old_a = temp;
}

pub fn extended_euclidean_algorithm(a: i32, b: i32) -> (i32, i32, i32) {
    let (mut old_r, mut rem) = (a, b);
    let (mut old_s, mut coeff_s) = (1, 0);
    let (mut old_t, mut coeff_t) = (0, 1);

    while rem != 0 {
        let quotient = old_r / rem;

        update_step(&mut rem, &mut old_r, quotient);
        update_step(&mut coeff_s, &mut old_s, quotient);
        update_step(&mut coeff_t, &mut old_t, quotient);
    }

    (old_r, old_s, old_t)
}

pub fn compute_symmetry(robots: &Vec<Robot>) -> (f32, f32) {
    let mut average_x = 0;
    let mut average_y = 0;
    for robot in robots.iter() {
        average_x += robot.x;
        average_y += robot.y;
    }

    let n = robots.len() as f32;

    let average_x: f32 = average_x as f32 / n;
    let average_y: f32 = average_y as f32 / n;

    let mut var_x: f32 = 0.0;
    let mut var_y: f32 = 0.0;

    for robot in robots.iter() {
        var_x += (robot.x as f32 - average_x).powf(2.0);
        var_y += (robot.y as f32 - average_y).powf(2.0);
    }

    let var_x = var_x / (n - 1.0);
    let var_y = var_y / (n - 1.0);

    return (var_x, var_y);
}

#[inline(always)]
pub fn simulate_robot(robot: &mut Robot, grid_width: i32, grid_height: i32, t: i32) {
    robot.x = (robot.x + t * robot.v_x).rem_euclid(grid_width);
    robot.y = (robot.y + t * robot.v_y).rem_euclid(grid_height);
}

pub fn count_quadrants(robots: &Vec<Robot>, grid_width: i32, grid_height: i32) -> i32 {
    // count the number of robots in each quadrant
    // grid width and height are odd
    // dont count the middle number

    let topl = robots
        .iter()
        .filter(|Robot { x, y, .. }| -> bool { x < &(grid_width / 2) && y < &(grid_height / 2) })
        .collect::<Vec<&Robot>>();

    let topr = robots
        .iter()
        .filter(|Robot { x, y, .. }| -> bool { x > &(grid_width / 2) && y < &(grid_height / 2) })
        .collect::<Vec<&Robot>>();

    let botl = robots
        .iter()
        .filter(|Robot { x, y, .. }| -> bool { x < &(grid_width / 2) && y > &(grid_height / 2) })
        .collect::<Vec<&Robot>>();

    let botr = robots
        .iter()
        .filter(|Robot { x, y, .. }| -> bool { x > &(grid_width / 2) && y > &(grid_height / 2) })
        .collect::<Vec<&Robot>>();

    let count = topl.len() * topr.len() * botl.len() * botr.len();
    return count as i32;
}

pub fn p1() -> u64 {
    let input = std::fs::read_to_string("../data/14.txt.fen").unwrap();
    let mut robots = parse_input(&input);

    let grid_width = 101;
    let grid_height = 103;

    // let grid_height = 7;
    // let grid_width = 11;

    let simulation_steps = 103;

    //animate_simulation(&mut robots, grid_width, grid_height, simulation_steps);
    let mut min_var_x = 10000000000.0;
    let mut min_var_y = 10000000000.0;
    let mut min_t_x = 0;
    let mut min_t_y = 0;

    for t in (0..103) {
        for mut robot in robots.iter_mut() {
            simulate_robot(&mut robot, grid_width, grid_height, 1);
        }

        let (var_x, var_y) = compute_symmetry(&robots);

        if var_x < min_var_x {
            min_var_x = var_x;
            min_t_x = t;
        }

        if var_y < min_var_y {
            min_var_y = var_y;
            min_t_y = t;
        }
    }

    // need to solve
    // t = min_t_x mod grid_width
    // t = min_t_y mod grid_height

    for m1 in -(101 * 103)..(101 * 103) {
        for m2 in -(101 * 103)..(101 * 103) {
            if (m1 * grid_width + m2 * grid_height) == 1 {
                println!("m1 = {}, m2 = {}", m1, m2);
            }
        }
    }

    // solution is
    // t = min_t_x * m2 * grid_height + min_t_y * m1 * grid_width

    println!(
        "t = {} * m2 * {} + {} * m1 * {}",
        min_t_x, grid_height, min_t_y, grid_width
    );

    // 19 * (-50) * 103 + 89 *(51) * 101

    println!("min x symmetry {} found at t_x = {}", min_var_x, min_t_x);
    println!("min y symmetry {} found at t_y = {}", min_var_y, min_t_y);

    let mut robots = parse_input(&input);

    print_grid(
        &mut robots,
        grid_width as usize,
        grid_height as usize,
        6887 + 1,
    );

    // let count = count_quadrants(&robots, grid_width, grid_height);
    //println!("{:?}", count);
    //println!("{:?}", robots);
    return 0;
}
