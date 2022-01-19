use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive (Debug, Eq, Ord, PartialEq, PartialOrd, Clone)]
struct Coordinate {
    x: i32,
    y: i32,
    z: i32
}

fn read_lines<P>(filename: P) -> Vec<String>
where P: AsRef<Path>, {
    let file = match File::open(filename) {
        Err(why) => panic!("Couldn't open: {}", why),
        Ok(file) => file,
    };
    let read_lines = io::BufReader::new(file).lines();
    let mut lines: Vec<String> = Vec::new();
    for read_line in read_lines {
        if let Ok(line) = read_line {
            lines.push(line);
        }
    }
    return lines;
}

fn parse_sensors(lines: Vec<String>) -> Vec<Vec<Coordinate>> {
    let mut sensors: Vec<Vec<Coordinate>> = Vec::new();
    let mut sensor: Vec<Coordinate> = Vec::new();
    for line in lines {
        if line == "" {
            sensors.push(sensor);
            sensor = Vec::new();
        } else if line.starts_with("---") {
            continue;
        } else {
            let coordinate_strings = line.split(",").collect::<Vec<&str>>();
            let x = coordinate_strings[0].parse::<i32>().unwrap();
            let y = coordinate_strings[1].parse::<i32>().unwrap();
            let z = coordinate_strings[2].parse::<i32>().unwrap();
            sensor.push(Coordinate{x, y, z});
        }
    }
    sensors.push(sensor);
    return sensors;
}

fn print_sensors(sensors: &Vec<Vec<Coordinate>>) {
    for sensor in sensors {
        println!("Sensor");
        for beacon in sensor {
            println!("{:?}", beacon);
        }
    }
}

fn compute_distance_matrix(sensor: &Vec<Coordinate>) -> Vec<Vec<Coordinate>> {
    let mut distance_matrix : Vec<Vec<Coordinate>> = Vec::new();
    let mut sorted_sensor = sensor.to_vec();
    sorted_sensor.sort();
    for i in 0..sorted_sensor.len() {
        
    }
    return distance_matrix;
}

fn apply_rotation(sensor: &Vec<Coordinate>, rotation: Coordinate) -> Vec<Coordinate> {
    // TODO: implement
}

fn compute_overlapping_beacons(sensor_one: Vec<Coordinate>, sensor_two: Vec<Coordinate>) -> u32 {
    // TODO: implement
}

fn main() {
    let sensors = parse_sensors(read_lines("input/input_19_1.txt"));
    let distance_matrices: Vec<Vec<Vec<Coordinate>>> = sensors.iter().map(compute_distance_matrix).collect();
}