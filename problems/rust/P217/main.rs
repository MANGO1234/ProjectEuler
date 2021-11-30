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

const M: u64 = 3u64.pow(15);

fn main() {
    let start = Instant::now();

    let mut have0p = vec![1u64];
    let mut have0sump = vec![0u64];
    let mut sum = 45;
    for d in 1..=(47 - 1) / 2 {
        let mut no0 = vec![0; have0p.len() + 9];
        let mut have0 = vec![0; have0p.len() + 9];
        let mut no0sum = vec![0; have0p.len() + 9];
        let mut have0sum = vec![0; have0p.len() + 9];
        for j in 0..have0p.len() {
            for i in 1..=9 {
                no0[j + i] = (no0[j + i] + have0p[j]) % M;
                no0sum[j + i] = (no0sum[j + i]
                    + i as u64 * pow_mod(10, d - 1, M as u32) * have0p[j]
                    + have0sump[j])
                    % M;
                have0[j + i] = (have0[j + i] + have0p[j]) % M;
                have0sum[j + i] = (have0sum[j + i]
                    + i as u64 * pow_mod(10, d - 1, M as u32) * have0p[j]
                    + have0sump[j])
                    % M;
            }
            have0[j] += have0p[j];
            have0sum[j] = (have0sum[j] + have0sump[j]) % M;
        }
        for ds in 0..no0.len() {
            // ds is digit_sum
            sum = (sum + have0[ds] * no0sum[ds] % M * pow_mod(10, d, M as u32)) % M;
            sum = (sum + no0[ds] * have0sum[ds]) % M;
            sum = (sum + have0[ds] * no0sum[ds] * 10 % M * pow_mod(10, d + 1, M as u32)) % M;
            sum = (sum + no0[ds] * have0sum[ds] * 10 % M) % M;
            sum = (sum + no0[ds] * have0[ds] % M * 45 * pow_mod(10, d, M as u32)) % M;
        }
        have0p = have0;
        have0sump = have0sum;
    }
    dbg!(sum);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
