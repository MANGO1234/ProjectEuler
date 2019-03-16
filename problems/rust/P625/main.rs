#![allow(unused_imports)]
mod euler;

use euler::eratosthenes_sieve::EratosthenesSieve;
use std::collections::HashMap;
use std::thread;
use std::time::Instant;

const N: u64 = 100000000000;
const M: u64 = 998244353;

fn totient_sum(n: u64, cache: &[u64], cache2: &mut HashMap<u64, u64>) -> u64 {
    if n as usize > cache.len() {
        let v = cache2.get(&n);
        if v.is_some() {
            return *v.unwrap();
        }
    }
    let mut sum = if n % 2 == 0 {
        (n / 2) % M * ((n + 1) % M) % M
    } else {
        n % M * ((n + 1) / 2 % M) % M
    };
    let sqrt = (n as f64).sqrt() as u64;
    for i in 2..=sqrt {
        let k = n / i;
        if k <= cache.len() as u64 {
            sum = sum + M - cache[k as usize - 1];
        } else {
            sum = sum + M - totient_sum(n / i, cache, cache2);
        }
    }
    for i in 1..(if n / sqrt == sqrt { sqrt } else { sqrt + 1 }) {
        sum = sum + M - (n / i - n / (i + 1)) * cache[i as usize - 1] % M;
    }
    if n as usize > cache.len() {
        cache2.insert(n, sum % M);
    }
    sum % M
}

fn main() {
    let start = Instant::now();

    // Dirichlet hyperbola method
    let cache_size = (N as f64).powf(2.0 / 3.0) as u64;
    let mut sieve = Vec::with_capacity(cache_size as usize);
    for i in 1..=cache_size {
        sieve.push(i);
    }
    for i in 2..=cache_size {
        if sieve[i as usize - 1] == i {
            for k in (i..=cache_size).step_by(i as usize) {
                sieve[k as usize - 1] = sieve[k as usize - 1] / i * (i - 1);
            }
        }
    }

    let mut cache = Vec::with_capacity(cache_size as usize);
    let mut sum = 0;
    // can also start at 2 and push 0 and don't subtract 1 at end
    for i in 1..=cache_size as usize {
        sum = (sum + sieve[i - 1]) % M;
        cache.push(sum);
    }

    let mut cache2 = HashMap::new();
    totient_sum(N, &cache, &mut cache2);

    // essentially can use totient sum in prev problem to find gcd sum
    // i.e. { (a, b) | 0 < a <= b < N, gcd(a, b) = k } has totient_sum(N / k) elements
    let mut sum = 0;
    let sqrt = (N as f64).sqrt() as u64;
    for n in 1..=sqrt {
        let d = N / n;
        if d as usize <= cache.len() {
            sum = (sum + n * cache[d as usize - 1]) % M;
        } else {
            sum = (sum + n * cache2.get(&d).unwrap()) % M;
        }
    }
    for n in 1..(if N / sqrt == sqrt { sqrt } else { sqrt + 1 }) {
        let a1 = N / n;
        let a2 = N / (n + 1);
        let subsum1 = if a1 % 2 == 0 {
            (a1 / 2) % M * ((a1 + 1) % M) % M
        } else {
            a1 % M * ((a1 + 1) / 2 % M) % M
        };
        let subsum2 = if a2 % 2 == 0 {
            (a2 / 2) % M * ((a2 + 1) % M) % M
        } else {
            a2 % M * ((a2 + 1) / 2 % M) % M
        };
        sum = (sum + (subsum1 + M - subsum2) * cache[n as usize - 1]) % M;
    }
    dbg!(sum);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
