#![allow(unused_imports)]
mod euler;

use crate::euler::math_utils::{
    mod_inv, n_c_r, pow_mod, unique_prime_factors_of, CombinationGenerator,
};
use core::fmt;
use euler::eratosthenes_sieve::EratosthenesSieve;
use itertools::Itertools;
use num::integer::{gcd, sqrt, Roots};
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

    let upto = (50000000f64 * 2f64.sqrt()) as u64;
    let sieve = EratosthenesSieve::new(upto as usize);
    let mut is_prime = vec![true; 50000000 + 1];
    is_prime[0] = false;
    is_prime[1] = false;
    let primes = sieve.get_vector_of_primes();
    let mut c = 0;
    for &p in &primes[1..] {
        if pow_mod(((p + 1) / 2) as u32, (p - 1) / 2, p as u32) != 1 {
            continue;
            // dbg!(p);
        }
        let mut rec = find_quad_recip(p);
        assert!(rec * rec % p == (p + 1) / 2);
        let mut n = if rec > p / 2 { p - rec } else { rec };
        let mut diff = vec![p - n * 2, 2 * n];
        let mut x = 0;
        while n < is_prime.len() as u64 {
            let f = 2 * n * n - 1;
            if f != p {
                is_prime[n as usize] = false;
            }
            n += diff[x];
            x = 1 - x;
        }
    }
    dbg!(sieve.num_of_primes());
    dbg!(is_prime.iter().filter(|x| **x).count());

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}

fn find_quad_recip(p: u64) -> u64 {
    if p % 4 == 3 {
        return pow_mod(((p + 1) / 2) as u32, (p + 1) / 4, p as u32);
    } else if p % 8 == 5 {
        let x = pow_mod(((p + 1) / 2) as u32, (p + 3) / 8, p as u32);
        if x * x % p == (p + 1) / 2 {
            return x;
        }
        return x * pow_mod(2, (p - 1) / 4, p as u32) % p;
    } else {
        let mut q = p - 1;
        let mut s = 0;
        while q % 2 == 0 {
            q /= 2;
            s += 1;
        }

        let mut z = 2;
        while z < p {
            if pow_mod(z as u32, (p - 1) / 2, p as u32) == p - 1 {
                break;
            }
            z += 1;
        }

        let n = (p + 1) / 2;
        let mut m = s;
        let mut c = pow_mod(z as u32, q, p as u32);
        let mut t = pow_mod(n as u32, q, p as u32);
        let mut r = pow_mod(n as u32, (q + 1) / 2, p as u32);

        loop {
            if t == 0 {
                return 0;
            } else if t == 1 {
                return r;
            }
            let mut i = 0;
            let mut tmp = t;
            while tmp != 1 {
                tmp = tmp * tmp % p;
                i += 1;
            }
            let b = pow_mod(c as u32, pow_mod(2, m - i - 1, p as u32), p as u32);
            let b2 = b * b % p;
            m = i;
            c = b2;
            t = t * b2 % p;
            r = r * b % p;
        }
    }
}
