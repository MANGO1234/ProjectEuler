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

fn main() {
    let start = Instant::now();

    let NN = 10000000000000u64;
    let mut i = 1u64;
    // let mut v = Vec::new();
    // while i * (i + 1) * i * (i + 1) <= NN {
    //     // q.push(Reverse((i * (i + 1) * i * (i + 1), i, i)));
    //     let mut j = i;
    //     while i * (i + 1) * j * (j + 1) <= NN {
    //         v.push(i * (i + 1) * j * (j + 1));
    //         j+=1;
    //     }
    //     i += 1;
    // }
    // v.sort();
    // v.dedup();
    // dbg!(v.len());

    // 3 ways
    // 1. add to vector, sort and dedup (thought this was slow, but it was the fastest)
    // 2. using a heap, essentially heap sort, uses least amount of memory (about 2x the runtime of method 3)
    // 3. dedup using a set (slowest, about 4x the runtime of method 1)
    let mut sum = 0;
    let mut q = BinaryHeap::new();
    while i * (i + 1) * i * (i + 1) <= NN {
        q.push(Reverse((i * (i + 1) * i * (i + 1), i, i)));
        i += 1;
    }
    let mut last = 0;
    while q.len() != 0 {
        let Reverse((cur, i, j)) = q.pop().unwrap();
        if cur != last {
            sum += 1;
            last = cur;
        }
        let n = i * (i + 1) * (j + 1) * (j + 2);
        if n <= NN {
            q.push(Reverse((n, i, j + 1)));
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
