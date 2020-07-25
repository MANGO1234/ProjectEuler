#![allow(unused_imports)]
mod euler;

use crate::euler::math_utils::mod_inv;
use crate::euler::math_utils::pow_mod;
use euler::eratosthenes_sieve::EratosthenesSieve;
use std::collections::HashMap;
use std::thread;
use std::time::Instant;

const M: u64 = 1000000007;
const REPEAT: u64 = 1_000_000_000_000;

fn main() {
    let start = Instant::now();

    let sieve = EratosthenesSieve::new(15485863);
    let mut prime_str = String::new();
    for prime in sieve.get_vector_of_primes() {
        prime_str = prime_str + &prime.to_string();
    }
    dbg!(prime_str.len());
    dbg!(sieve.get_vector_of_primes().len());

    let t = REPEAT % M;
    let c = prime_str.len() as u64;
    let p10c = pow_mod(10, c, M as u32) as u64;
    let mod_inv_9 = mod_inv(9, M as u32) as u64;
    let mod_inv_p10c = mod_inv(p10c as u32 - 1, M as u32) as u64;
    let part1 = p10c * (pow_mod(10, c * (REPEAT - 1), M as u32) - 1);
    let part1 = (part1 + M - (t - 1) * (p10c - 1) % M) % M;
    let part1 = c * part1 % M * mod_inv_p10c % M * mod_inv_p10c % M;
    let part2 = (pow_mod(10, c * REPEAT, M as u32) - 1) * mod_inv_p10c % M;
    let part3 = if t % 2 == 0 {
        c * (t / 2) % M * (t - 1) % M
    } else {
        c * t % M * ((t - 1) / 2) % M
    };
    // pow_mod(10, a + 1, M as u32) but change to a loop = faster
    let mod_inv_10 = mod_inv(10, M as u32) as u64;
    let mut p10aplus1 = pow_mod(10, c, M as u32) as u64;

    let mut sum = 0;
    for b in 0..c {
        let digit = prime_str.as_bytes()[b as usize] as u64 - 48;
        let mut sub_sum = 0;
        sub_sum += p10aplus1 * part1 % M;
        sub_sum += (b + 1) * p10aplus1 % M * part2 % M;
        sub_sum = (sub_sum + M - part3) % M;
        sub_sum = (sub_sum + M - t * (b + 1) % M) % M;
        sub_sum = sub_sum * mod_inv_9 % M;
        p10aplus1 = p10aplus1 * mod_inv_10 % M;
        sum = (sum + sub_sum * digit) % M;
    }
    dbg!(sum % M);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
