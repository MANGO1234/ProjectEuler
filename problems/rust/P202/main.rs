#![allow(unused_imports)]
mod euler;

use crate::euler::math_utils::{
    mod_inv, n_c_r, pow_mod, unique_prime_factors_of, CombinationGenerator,
};
use core::fmt;
use euler::eratosthenes_sieve::EratosthenesSieve;
use itertools::Itertools;
use num::integer::{gcd, Roots};
#[cfg(feature = "bigint")]
use num::ToPrimitive;
#[cfg(feature = "bigint")]
use num::{BigInt, BigRational, FromPrimitive};
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::thread;
use std::time::Instant;

fn method1(nn: u64) -> u64 {
    let mut answer = 0;
    let row = (nn + 3) / 2;
    if row % 3 == 0 {
        return 0;
    }
    let factors = unique_prime_factors_of(row as u64);
    let product = factors.iter().fold(1, |x, &b| x * b);
    let totient = factors.iter().fold(1, |x, &b| x * (b - 1));
    let mut current = 3 - (row % 3);
    while current + product * 3 < row {
        current += product * 3;
        answer += totient;
    }

    for i in (current..row).step_by(3) {
        if gcd(i, row) == 1 {
            answer += 1;
        }
    }
    return answer;
}

fn method2(nn: u64) -> u64 {
    let mut answer = 0;
    let row = (nn + 3) / 2;
    if row % 3 == 0 {
        return 0;
    }
    let factors = unique_prime_factors_of(row as u64);
    let product = factors.iter().fold(1, |x, &b| x * b);
    let totient = factors.iter().fold(1, |x, &b| x * (b - 1));
    let mut current = 3 - (row % 3);
    while current + product * 3 < row {
        current += product * 3;
        answer += totient;
    }

    let mut counter = vec![0; factors.len()];
    for i in 0..counter.len() {
        counter[i] = (3 - (row % 3)) % factors[i];
    }
    for _ in (current..row).step_by(3) {
        if !counter.iter().any(|x| *x == 0) {
            answer += 1;
        }
        for i in 0..counter.len() {
            counter[i] = (counter[i] + 3) % factors[i];
        }
    }
    return answer;
}

// inclusion/exclusion
fn method3(nn: u64) -> u64 {
    let mut answer = 0;
    let mut sub = 0;
    let row = (nn + 3) / 2;
    if row % 3 == 0 {
        return 0;
    }
    let factors = unique_prime_factors_of(row as u64);
    for i in 1i32..(1 << factors.len()) {
        let mut product = 1;
        for x in 0..factors.len() {
            if (i >> x) % 2 == 1 {
                product *= factors[x];
            }
        }
        let s = 3 - (row % 3);
        let mut c = (row - s) / (product * 3) * (product - 1);
        c += ((row - s) % (product * 3) + 2) / 3;
        let rem = (row - s) % (product * 3);
        if rem >= product && product >= s {
            if (product - s) % 3 == 0 {
                c -= 1;
            }
        }
        if rem >= product * 2 {
            if (product * 2 - s) % 3 == 0 {
                c -= 1;
            }
        }
        if i.count_ones() % 2 == 1 {
            answer += c;
        } else {
            sub += c;
        }
    }
    return answer - sub;
}

fn main() {
    let start = Instant::now();

    dbg!(method3(12017639147));

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
