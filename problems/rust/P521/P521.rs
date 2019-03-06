#![allow(unused_imports)]
mod euler;

use euler::eratosthenes_sieve::EratosthenesSieve;
use std::cmp::max;
use std::thread;
use std::time::Instant;

const M : u64 = 1000000000;

fn sum_smallest(n: u64) -> (u64, (Vec<u64>, Vec<u64>, Vec<u64>)) {
    let sqrt = (n as f64).sqrt() as u64;
    let mut x = Vec::with_capacity(sqrt as usize * 2);
    let mut f = Vec::with_capacity(sqrt as usize * 2);
    let mut count = Vec::with_capacity(sqrt as usize * 2);
    let mut prev = 0;
    let mut sum = 0;
    for i in 0..sqrt {
        x.push(i + 1);
        f.push((i + 1) * (i + 2) / 2 - 1);
        count.push(i);
    }
    let start = if n / sqrt == sqrt { 1 } else { 0 };
    for i in start..sqrt {
        x.push(n / (sqrt - i));
        let k = n / (sqrt - i);
        if k % 2 == 0 {
            f.push(((k / 2 % M) * ((k + 1) % M) + M - 1) % M);
        } else {
            f.push(((k % M) * ((k + 1) / 2 % M) + M - 1) % M);
        }
        count.push(n / (sqrt - i) - 1);
    }
    let end = f.len();
    for i in 1..sqrt as usize {
        let p = i + 1;
        if f[i] != f[i - 1] {
            let mut j = f.len() - 1;
            while x[j] >= (p * p) as u64 {
                // find x[k] = j / p
                let d = x[j] / p as u64;
                let k = if d <= sqrt {
                    d as usize - 1
                } else {
                    end - (end - j) * p as usize
                };
                f[j] = (f[j] + M - (p as u64 * ((f[k] + M - f[i - 1]) % M)) % M) % M;
                count[j] -= count[k] - count[i - 1];
                j -= 1;
            }
            sum += p as u64 * ((n - 1) - count[count.len() - 1] - prev); // the composites with p as smallest factor
            prev = (n - 1) - count[count.len() - 1];
        }
    }
    sum += f[f.len() - 1]; // the primes themselves
    return (sum, (x, count, f));
}

fn main() {
    let start = Instant::now();

    dbg!(sum_smallest(100).0 % M);
    dbg!(sum_smallest(1000000000000).0 % M);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
