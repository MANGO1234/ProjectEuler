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

    let mut k = 0u64;
    let mut last = u64::MAX;
    let mut sum = 0;
    while k != 2 {
        k = (k + 1504170715041707) % 4503599627370517;
        if k < last {
            sum += k;
            last = k;
            if last < 20000000 {
                break;
            }
        }
    }
    let u = last;
    k = 0;
    last = u64::MAX;
    for i in 1..u {
        // 3451657199285664 is inverse of 1504170715041707
        k = (k + 3451657199285664) % 4503599627370517;
        if k < last {
            last = k;
            sum += i;
        }
    }
    dbg!(sum);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
