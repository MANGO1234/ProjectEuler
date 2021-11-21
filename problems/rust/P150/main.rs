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

    // brute force
    let mut tri = Vec::new();
    let mut t = 0i32;
    for i in 1..=1000 {
        let mut v = Vec::new();
        v.push(0);
        for i in 0..i {
            t = ((615949 * t as i64 + 797807) % (1 << 20)) as i32;
            // dbg!(v[i]);
            // dbg!(t - (1 << 19));
            v.push(t - (1 << 19) + v[i]);
        }
        tri.push(v);
    }
    let mut min = i32::MAX;
    for i in 0..tri.len() {
        for col in 0..=i {
            let mut sum = 0;
            for row in i..tri.len() {
                sum += tri[row][col + (row + 1 - i)] - tri[row][col];
                min = min.min(sum);
            }
        }
    }
    dbg!(min);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
