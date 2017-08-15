mod euler;

use std::time::Instant;
use euler::math_utils::*;

#[derive(Clone, Copy, Debug)]
pub struct Digit {
    remaining: u32,
    chosen_digit: i32,
    not_possible: [bool; 10],
}

impl Digit {
    pub fn new() -> Digit {
        Digit {
            remaining: 10,
            chosen_digit: -1,
            not_possible: [false; 10],
        }
    }
}

const GUESSES: [(u32, [i32; 16]); 22] = [
    (0, [2, 3, 2, 1, 3, 8, 6, 1, 0, 4, 3, 0, 3, 8, 4, 5]),
    (1, [3, 1, 7, 4, 2, 4, 8, 4, 3, 9, 4, 6, 5, 8, 5, 8]),
    (1, [3, 8, 4, 7, 4, 3, 9, 6, 4, 7, 2, 9, 3, 0, 4, 7]),
    (1, [4, 8, 9, 5, 7, 2, 2, 6, 5, 2, 1, 9, 0, 3, 0, 6]),
    (1, [6, 3, 7, 5, 7, 1, 1, 9, 1, 5, 0, 7, 7, 0, 5, 0]),
    (1, [6, 9, 1, 3, 8, 5, 9, 1, 7, 3, 1, 2, 1, 3, 6, 0]),
    (1, [8, 1, 5, 7, 3, 5, 6, 3, 4, 4, 1, 1, 8, 4, 8, 3]),
    (2, [2, 3, 2, 6, 5, 0, 9, 4, 7, 1, 2, 7, 1, 4, 4, 8]),
    (2, [2, 6, 1, 5, 2, 5, 0, 7, 4, 4, 3, 8, 6, 8, 9, 9]),
    (2, [2, 6, 5, 9, 8, 6, 2, 6, 3, 7, 3, 1, 6, 8, 6, 7]),
    (2, [4, 5, 1, 3, 5, 5, 9, 0, 9, 4, 1, 4, 6, 1, 1, 7]),
    (2, [5, 2, 5, 1, 5, 8, 3, 3, 7, 9, 6, 4, 4, 3, 2, 2]),
    (2, [5, 6, 1, 6, 1, 8, 5, 6, 5, 0, 5, 1, 8, 2, 9, 3]),
    (2, [6, 4, 4, 2, 8, 8, 9, 0, 5, 5, 0, 4, 2, 7, 6, 8]),
    (3, [1, 7, 4, 8, 2, 7, 0, 4, 7, 6, 7, 5, 8, 2, 7, 6]),
    (3, [1, 8, 4, 1, 2, 3, 6, 4, 5, 4, 3, 2, 4, 5, 8, 9]),
    (3, [3, 0, 4, 1, 6, 3, 1, 1, 1, 7, 2, 2, 4, 6, 3, 5]),
    (3, [4, 2, 9, 6, 8, 4, 9, 6, 4, 3, 6, 0, 7, 5, 4, 3]),
    (3, [5, 8, 5, 5, 4, 6, 2, 9, 4, 0, 8, 1, 0, 5, 8, 7]),
    (3, [7, 8, 9, 0, 9, 7, 1, 5, 4, 8, 9, 0, 8, 0, 6, 7]),
    (3, [8, 6, 9, 0, 0, 9, 5, 8, 5, 1, 5, 2, 6, 2, 5, 4]),
    (3, [9, 7, 4, 2, 8, 5, 5, 5, 0, 7, 0, 6, 8, 3, 5, 3]),
];

pub fn f(i: usize, digits: &mut [Digit; 16], j: usize, mut n_rem: i32) -> i32 {
    if i == GUESSES.len() {
        for digit in digits {
            print!("{:?}", digit.chosen_digit);
        }
        println!();
        return 0;
    }
    if n_rem < 0 {
        n_rem = GUESSES[i].0 as i32;
    }
    if j == 16 {
        if n_rem > 0 {
            return -1;
        }
        return f(i + 1, digits, 0, -1);
    }
    let d = GUESSES[i].1[j];
    if digits[j].chosen_digit >= 0 {
        if digits[j].chosen_digit == d {
            if n_rem == 0 {
                return -1;
            } else {
                return f(i, digits, j + 1, n_rem - 1);
            }
        } else {
            return f(i, digits, j + 1, n_rem);
        }
    } else {
        if digits[j].not_possible[d as usize] {
            return f(i, digits, j + 1, n_rem);
        } else {
            digits[j].not_possible[d as usize] = true;
            digits[j].remaining -= 1;
            if digits[j].remaining == 1 {
                for i in 0..digits[j].not_possible.len() {
                    if !digits[j].not_possible[i as usize] {
                        digits[j].chosen_digit = i as i32;
                        break;
                    }
                }
            }
            let r = f(i, digits, j + 1, n_rem);
            if digits[j].remaining == 1 {
                digits[j].chosen_digit = -1;
            }
            digits[j].remaining += 1;
            digits[j].not_possible[d as usize] = false;
            if r >= 0 {
                return r;
            }
            if n_rem > 0 {
                digits[j].chosen_digit = d;
                let r = f(i, digits, j + 1, n_rem - 1);
                digits[j].chosen_digit = -1;
                if r >= 0 {
                    return r;
                }
            }
            return -1;
        }
    }
}

fn main() {
    let start = Instant::now();

    let mut digits = [Digit::new(); 16];
    println!("{}", f(0, &mut digits, 0, -1));

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
