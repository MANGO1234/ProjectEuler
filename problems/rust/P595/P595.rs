#![allow(unused_imports)]
mod euler;

use crate::euler::math_utils::{factorial_big, mod_inv, n_c_r, n_c_r_big, pow_mod};
use euler::eratosthenes_sieve::EratosthenesSieve;
#[cfg(feature = "bigint")]
use num::ToPrimitive;
#[cfg(feature = "bigint")]
use num::{BigInt, BigRational, FromPrimitive};
use std::collections::HashMap;
use std::thread;
use std::time::Instant;

const N: usize = 52;

fn main() {
    let start = Instant::now();

    // S = number of permutation of k sticky segment in 1..=n
    // E = expected count of sort
    // 1. S(n,k<n)=(n-1)C(k-1) * S(k,k)
    // 2. S(n,n) = n! - S(n,k<n)
    // 3. E(n) = (S(n,k<n)(1+E(k)) + S(n, n)) / (n! - S(n,n))
    // let mut s = vec![Vec::new(); N + 1];
    // let mut e = Vec::with_capacity(N);
    // e.push(BigRational::from_i32(0).unwrap());
    // e.push(BigRational::from_i32(0).unwrap());
    // for n in 1..s.len() {
    //     let mut e_sum = BigRational::from_i32(0).unwrap();
    //     let mut sum = BigInt::from(0);
    //     s[n].push(BigInt::from(0));
    //     for k in 1..n {
    //         let t = n_c_r_big(n as u64 - 1, k as u64 - 1) * &s[k][k];
    //         e_sum += (e[k].clone() + BigInt::from(1)) * &t;
    //         sum += &t;
    //         s[n].push(t);
    //     }
    //     s[n].push(factorial_big(n as u64) - &sum);
    //     e_sum += &s[n][n];
    //     if n > 1 {
    //         e.push(e_sum / sum);
    //     }
    // }
    // for i in 0..e.len() {
    //     // subtract 1 because the question starts with equal likelihood of every permutation
    //     // the first shuffle is from 7,6,5,4,3,2,1 to get to there
    //     println!(
    //         "{}={}",
    //         i,
    //         e[i].to_f64().unwrap() - 1.0 //
    //                                      // &e[i] - BigRational::from_i32(1).unwrap()
    //     );
    // }

    // floating point implementation, much faster but slightly off (very small diff)
    let mut s = vec![Vec::new(); N + 1];
    let mut e = Vec::with_capacity(N);
    e.push(0.0);
    e.push(0.0);
    for n in 1..s.len() {
        let mut e_sum = 0.0;
        let mut sum = 0.0;
        s[n].push(0.0);
        for k in 1..n {
            let mut t = n_c_r(n as u64 - 1, k as u64 - 1) as f64 * s[k][k];
            for j in k + 1..=n {
                t /= j as f64;
            }
            e_sum += (e[k] + 1.0) * t;
            sum += t;
            s[n].push(t);
        }
        s[n].push(1.0 - sum);
        e_sum += s[n][n];
        if n > 1 {
            e.push(e_sum / sum);
        }
    }
    for i in 0..e.len() {
        // subtract 1 because the question starts with equal likelihood of every permutation
        // the first shuffle is from 7,6,5,4,3,2,1 to get to there
        println!("{}={}", i, e[i] - 1.0);
    }

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
