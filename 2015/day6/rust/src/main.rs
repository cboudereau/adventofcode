use regex::Regex;

#[derive(Debug)]
enum Action {
    TurnOn,
    TurnOff,
    Toggle,
}

fn part1(input: &str) -> i32 {
    let input = input.split("\r\n");
    let regex = Regex::new(r"([A-Za-z\s]+)\s(\d+),(\d+)\sthrough\s(\d+),(\d+)").unwrap();
    let mut matrix = vec![vec![false; 1000]; 1000];

    for line in input {
        for (_, [action, xmin, ymin, xmax, ymax]) in regex.captures_iter(line).map(|c| c.extract())
        {
            let action = match action {
                "turn on" => Action::TurnOn,
                "turn off" => Action::TurnOff,
                "toggle" => Action::Toggle,
                other => panic!("unexpected '{other}' line"),
            };

            let xmin = xmin.parse::<usize>().unwrap();
            let ymin = ymin.parse::<usize>().unwrap();
            let xmax = xmax.parse::<usize>().unwrap();
            let ymax = ymax.parse::<usize>().unwrap();

            // optimized by running cargo clippy --all-targets --all-features -- -D warnings
            for row in matrix.iter_mut().take(ymax + 1).skip(ymin) {
                for cell in row.iter_mut().take(xmax + 1).skip(xmin) {
                    *cell = match action {
                        Action::TurnOn => true,
                        Action::TurnOff => false,
                        Action::Toggle => !*cell,
                    }
                }
            }
        }
    }

    let mut counter = 0;
    for row in matrix.iter() {
        for cell in row {
            if *cell {
                counter += 1;
            }
        }
    }

    counter
}

fn part2(input: &str) -> i32 {
    let input = input.split("\r\n");
    let regex = Regex::new(r"([A-Za-z\s]+)\s(\d+),(\d+)\sthrough\s(\d+),(\d+)").unwrap();
    let mut matrix = vec![vec![0; 1000]; 1000];

    for line in input {
        for (_, [action, xmin, ymin, xmax, ymax]) in regex.captures_iter(line).map(|c| c.extract())
        {
            let action = match action {
                "turn on" => Action::TurnOn,
                "turn off" => Action::TurnOff,
                "toggle" => Action::Toggle,
                other => panic!("unexpected '{other}' line"),
            };

            let xmin = xmin.parse::<usize>().unwrap();
            let ymin = ymin.parse::<usize>().unwrap();
            let xmax = xmax.parse::<usize>().unwrap();
            let ymax = ymax.parse::<usize>().unwrap();

            // optimized by running cargo clippy --all-targets --all-features -- -D warnings
            for row in matrix.iter_mut().take(ymax + 1).skip(ymin) {
                for previous in row.iter_mut().take(xmax + 1).skip(xmin) {
                    *previous = match action {
                        Action::TurnOn => *previous + 1,
                        Action::TurnOff => {
                            if *previous > 0 {
                                *previous - 1
                            } else {
                                *previous
                            }
                        }
                        Action::Toggle => *previous + 2,
                    }
                }
            }
        }
    }

    let mut counter = 0;
    for row in matrix.into_iter() {
        for cell in row {
            counter += cell;
        }
    }

    counter
}

fn main() {
    let input = include_str!("../../day6.txt");

    let part1 = part1(input);
    println!("part1: {part1}");

    let part2 = part2(input);
    println!("part2: {part2}");
}

#[cfg(test)]
#[test]
fn test_part1() {
    let input = include_str!("../../day6.txt");
    assert_eq!(377891, part1(input))
}

#[test]
fn test_part2() {
    let input = include_str!("../../day6.txt");
    assert_eq!(14110788, part2(input))
}

#[test]
fn test_stack_alloc() {
    // Stack alloc array example
    fn get<const N: usize, T: Copy>(mat: &[[T; N]; N], i: usize, j: usize) -> T {
        mat[i][j]
    }

    let mut m = [[false; 10]; 10];

    m[0][0] = true;

    assert!(get(&m, 0, 0))
}
