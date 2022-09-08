#![allow(unused_imports)]
mod euler;

use crate::euler::math_utils::{
    factorial, mod_inv, n_c_r, pow_mod, unique_prime_factors_of, CombinationGenerator,
};
use core::fmt;
use euler::eratosthenes_sieve::EratosthenesSieve;
use itertools::Itertools;
#[cfg(feature = "bigint")]
use num::ToPrimitive;
#[cfg(feature = "bigint")]
use num::{BigInt, BigRational, FromPrimitive};
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::thread;
use std::time::Instant;

const N: u64 = 1000000000;
const M: u64 = 1234567891;

fn main() {
    let start = Instant::now();

    let mut d = (1..=97).map(|x| x as f64).product::<f64>();
    let mut d2 = d;
    let mut sign = -1.0;
    for i in 1..=22 {
        let i = i as f64;
        d2 /= 97.0 - i + 1.0;
        d2 *= 22.0 - i + 1.0;
        d2 /= i;
        d += sign * d2;
        sign *= -1.0;
    }
    d *= 25.0 * 24.0 * 23.0 / 3.0 / 2.0;
    for i in 1..=100 {
        d /= i as f64;
    }
    dbg!(d);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
