#![allow(unused_imports)]
mod euler;

use euler::eratosthenes_sieve::EratosthenesSieve;
use std::collections::HashMap;
use std::thread;
use std::time::Instant;

const N: u64 = 50000000000;
const M: u64 = 1000000007;

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
    let mut sum = totient_sum(N, &cache, &mut cache2) - 1;
    let mut k = N / 2;
    while k > 1 {
        if k as usize <= cache.len() {
            sum += cache[k as usize - 1] - 1;
        } else {
            sum += cache2.get(&k).unwrap() - 1;
        }
        k /= 2;
    }
    dbg!(sum % M);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
