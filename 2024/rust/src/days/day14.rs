#[derive(Debug)]
struct Robot {
    x: i32,
    y: i32,
    v_x: i32,
    v_y: i32,
}

pub fn parse_input(input: &str) -> Vec<Robot> {
    // robot line looks like
    // p=28,6 v=82,93
    // split on " " to get the parts
    // split on "," to get the numbers
    // parse the numbers
    // return a Robot struct
    // return a Vec<Robot>
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

pub fn compute_symmetry(robots: &Vec<Robot>) -> i32 {
    let mut symmetry = 0;
    for robot in robots.iter() {
        let x_sym = robot.x;
        if robots.iter().filter(|Robot { x, .. }| *x == x_sym).count() == 2 {
            symmetry += 1;
        }
    }
    return symmetry;
}

pub fn simulate_robot(robot: &mut Robot, grid_width: i32, grid_height: i32, t: i32) {
    robot.x = (robot.x + t * robot.v_x).rem_euclid(grid_width);
    robot.y = (robot.y + t * robot.v_y).rem_euclid(grid_height);
}    

pub fn count_quadrants(robots: &Vec<Robot>, grid_width: i32, grid_height: i32) -> i32 {
    // count the number of robots in each quadrant
    // grid width and height are odd 
    // dont count the middle number 

    let topl = robots.iter().filter( |Robot { x, y, .. }| -> bool {
        x < &(grid_width / 2 ) && y < &(grid_height / 2 ) 
    }).collect::<Vec<&Robot>>();

    let topr = robots.iter().filter( |Robot { x, y, .. }| -> bool {
        x > &(grid_width / 2 ) && y < &(grid_height / 2 ) 
    }).collect::<Vec<&Robot>>();

    let botl = robots.iter().filter( |Robot { x, y, .. }| -> bool {
        x < &(grid_width / 2 ) && y > &(grid_height / 2 ) 
    }).collect::<Vec<&Robot>>();

    let botr = robots.iter().filter( |Robot { x, y, .. }| -> bool {
        x > &(grid_width / 2 ) && y > &(grid_height / 2 ) 
    }).collect::<Vec<&Robot>>();

    let count = topl.len() * topr.len() * botl.len()*  botr.len();
    return count as i32;
}

pub fn p1() -> u64 {
    let input = std::fs::read_to_string("../data/14.txt.fen").unwrap();
    let mut robots = parse_input(&input);

    let grid_height = 103;
    let grid_width = 101;

    // let grid_height = 7; 
    // let grid_width = 11;

    let simulation_steps = 1000000;

    // for mut robot in robots.iter_mut() {
    //     simulate_robot(&mut robot, grid_width, grid_height, 100);
    // }
    
    let mut max_symmetry = 0;

    for t in 0..simulation_steps {
        
        let symmetry = compute_symmetry(&robots);
        if symmetry > max_symmetry {
            max_symmetry = symmetry;
            println!("max symmetry {} found at t = {}", symmetry, t);
        }
        for mut robot in robots.iter_mut() {
            simulate_robot(&mut robot, grid_width, grid_height, 1);
        }
    }

    let count = count_quadrants(&robots, grid_width, grid_height);
    println!("{:?}", count);
    //println!("{:?}", robots);
    return 0;
}
