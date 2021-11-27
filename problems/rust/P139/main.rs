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

fn main() {
    let start = Instant::now();

    let mut m: i64 = 2;
    let mut sum = 0;
    while 2 * m * (m + 1) < 100000000 {
        let start = if m % 2 == 0 { 1 } else { 0 };
        for n in (start..m).step_by(2) {
            if 2 * m * (m + n) >= 100000000 {
                break;
            }
            if gcd(m, n) != 1 {
                continue;
            }
            if (m * m + n * n) % ((m * m - n * n - 2 * m * n) as i64).abs() == 0 {
                sum += 100000000 / (2 * m * (m + n));
            }
        }
        m += 1;
    }
    dbg!(sum);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
