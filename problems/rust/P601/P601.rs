#![allow(unused_imports)]
mod euler;

use euler::eratosthenes_sieve::EratosthenesSieve;
use euler::math_utils::gcd;
use std::cmp::max;
use std::thread;
use std::time::Instant;

fn main() {
    let start = Instant::now();

    let mut lcm = 1;
    let mut next_lcm = 1;
    let mut count = 0;
    for i in 1..=31 {
        let n = 1 << (2 * i);
        lcm = lcm / gcd(lcm , i) * i;
        next_lcm = next_lcm / gcd(next_lcm , i + 1) * (i + 1);
        // (n - 1) / lcm + 1 give the correct count, but need to skip 1 (don't add 1) and N (subtract 2)
        count += (n - 2) / lcm - (n - 2) / next_lcm;
    }
    dbg!(count);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
