#![allow(unused_imports)]
mod euler;

use euler::eratosthenes_sieve::EratosthenesSieve;
use euler::math_utils::gcd;
use std::cmp::max;
use std::thread;
use std::time::Instant;

const N: u64 = 1000000000000;

fn main() {
    let start = Instant::now();

    // let f(n) be number of 2 in prime factorization of n, E(m, n) = f((n+1)^m((n+2)^m-2^m)/2^m)
    // if n+1 is even, then E(m,n) = m*(f(n+1)-1), and count it
    let mut count_even = 0;
    // start from 2 because of -1
    for i in 2..=53 {
        count_even += (N + 1) / (1u64 << i) * 904961;
    }
    dbg!(count_even);

    // if n+1 is odd, n+2 is even and some steps later show that E(m,n) = f((n+2)/2)
    let mut count_odd = 0;
    for i in 1..=53 {
        count_odd += ((N + 2) / 2) / (1u64 << i);
    }
    dbg!(count_odd);
    dbg!(count_even + count_odd);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
