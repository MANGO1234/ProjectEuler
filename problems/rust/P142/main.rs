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

    let mut m:HashMap<i64, Vec<i64>> = HashMap::new();
    for i in 1..10000 {
        let mut found = false;
        for j in 1..i {
            if (i * i - j * j) % 2 == 0 {
                let y = (i * i - j * j) / 2;
                let x = j * j + y;
                match m.get_mut(&x) {
                    Some(v) => {
                        for &mut z in v.into_iter() {
                            // println!("{} {} {} {}", x, y, z, x+y+z);
                            if is_square(y + z) && is_square(y - z) {
                                println!("{} {} {} {}", x, y, z, x+y+z);
                                found = true;
                                break;
                            }
                        }
                        v.push(y);
                    },
                    None => {
                        let mut v = Vec::new();
                        v.push(y);
                        m.insert(x, v);
                    }
                }
            }
        }
        if found {
            break;
        }
    }
    dbg!(m.len());

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}

fn is_square(n: i64) -> bool {
    if (n < 0) {
        return false;
    }
    n.sqrt() * n.sqrt() == n
}
