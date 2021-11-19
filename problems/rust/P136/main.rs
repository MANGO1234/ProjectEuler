#![allow(unused_imports)]
mod euler;

use crate::euler::math_utils::{mod_inv, n_c_r, pow_mod, CombinationGenerator};
use core::fmt;
use std::cmp::Reverse;
use euler::eratosthenes_sieve::EratosthenesSieve;
use itertools::Itertools;
#[cfg(feature = "bigint")]
use num::ToPrimitive;
#[cfg(feature = "bigint")]
use num::{BigInt, BigRational, FromPrimitive};
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::thread;
use std::time::Instant;
use num::integer::Roots;

fn main() {
    let start = Instant::now();

    let mut a = vec![false; 50000001];
    let mut b = vec![false; 50000001];
    for i in 2..(50000000 as usize) {
        let mut s = 4 - (i % 4);
        for _k in ((s + i) / 4)..i {
            let p = s * i;
            if p > 50000000 {
                break;
            }
            if a[p] {
                b[p] = true;
            } else {
                a[p] = true;
            }
            s += 4;
        }
    }
    let mut sum = 0;
    for i in 0..a.len() {
        if a[i] && !b[i] {
            sum+=1;
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
