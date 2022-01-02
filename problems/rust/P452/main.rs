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

struct Cache {
    nCr_fact: Vec<u64>,
    fact: Vec<u64>,
    fact_inv: Vec<u64>,
}

fn main() {
    let start = Instant::now();

    let mut cache = Cache {
        nCr_fact: vec![0; ((N as f64).log(2.0) + 1.0) as usize],
        fact: vec![0; ((N as f64).log(2.0) + 1.0) as usize],
        fact_inv: vec![0; ((N as f64).log(2.0) + 1.0) as usize],
    };
    cache.nCr_fact[0] = 1;
    cache.fact[0] = 1;
    cache.fact_inv[0] = 1;
    for i in 1..cache.nCr_fact.len() {
        cache.nCr_fact[i] = cache.nCr_fact[i - 1] * (N - i as u64 + 1) % M;
        cache.fact[i] = cache.fact[i - 1] * i as u64 % M;
        cache.fact_inv[i] = mod_inv(cache.fact[i] as u32, M as u32);
    }
    // let mut counts = Vec::new();
    // dbg!(count(N as u32, 2, &mut counts, &cache) + 1);
    dbg!(count2(N as u32, 2, 1, 0, 0, 1, &cache) + 1);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}

fn count(mut n: u32, cur: u32, counts: &mut Vec<(u32, u64)>, cache: &Cache) -> u64 {
    let mut sum = 0;
    if counts.len() == 0 {
        counts.push((2, 1));
        sum = (sum + calc(counts, cache)) % M;
        counts.pop();
    } else {
        let l = counts.len() - 1;
        counts[l].1 += 1;
        sum = (sum + calc(counts, cache)) % M;
        counts[l].1 -= 1;
    }
    counts.push((1, 1));
    sum = (sum + (n - cur) as u64 * calc(counts, cache) % M) % M;
    counts.pop();
    for i in cur..=((n as f64).sqrt()) as u32 {
        if counts.len() == 0 || counts[counts.len() - 1].0 != i {
            counts.push((i, 1));
            sum = (sum + count(n / i, i, counts, cache)) % M;
            counts.pop();
        } else {
            let l = counts.len() - 1;
            counts[l].1 += 1;
            sum = (sum + count(n / i, i, counts, cache)) % M;
            counts[l].1 -= 1;
        }
    }
    return sum;
}

fn calc(counts: &mut Vec<(u32, u64)>, cache: &Cache) -> u64 {
    let sum: usize = counts.iter().map(|x| x.1).sum::<u64>() as usize;
    let mut p = cache.nCr_fact[sum];
    for x in counts {
        p = p * cache.fact_inv[x.1 as usize] % M;
    }
    return p;
}

// optimized version of count -> calc is done incrementally in the recursion
fn count2(
    mut n: u32,
    cur: u32,
    level: usize,
    last: u32,
    count: usize,
    p: u64,
    cache: &Cache,
) -> u64 {
    let mut sum = p * cache.fact_inv[count + 1] % M * cache.nCr_fact[level] % M;
    let mut tmp = p * cache.fact_inv[count] % M * cache.nCr_fact[level] % M;
    sum = (sum + (n - cur) as u64 * tmp % M) % M;

    for i in cur..=((n as f64).sqrt()) as u32 {
        if last != i {
            let np = p * cache.fact_inv[count] % M;
            sum = (sum + count2(n / i, i, level + 1, i, 1, np, cache)) % M;
        } else {
            sum = (sum + count2(n / i, i, level + 1, last, count + 1, p, cache)) % M;
        }
    }
    return sum;
}
