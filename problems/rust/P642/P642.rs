#![allow(unused_imports)]
mod euler;

use euler::eratosthenes_sieve::EratosthenesSieve;
use std::cmp::max;
use std::thread;
use std::time::Instant;

const M : u64 = 1000000000;

// todo: bad asymptotic, need to get a solution below 1 minute

fn sum_primes(n: u64) -> (u64, (Vec<u64>, Vec<u64>, Vec<u64>)) {
    let sqrt = (n as f64).sqrt() as u64;
    let mut x = Vec::with_capacity(sqrt as usize * 2);
    let mut f = Vec::with_capacity(sqrt as usize * 2);
    let mut count = Vec::with_capacity(sqrt as usize * 2);
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
        }
    }
    return (f[f.len() - 1], (x, count, f));
}

fn sum_primes2(n: u64, count: &[u64], primes: &[u64]) -> (u64, (Vec<u64>, Vec<u64>)) {
    let sqrt = (n as f64).sqrt() as u64;
    let mut x = Vec::with_capacity(sqrt as usize * 2);
    let mut f = Vec::with_capacity(sqrt as usize * 2);
    let mut sum = 0;
    for i in 0..sqrt {
        x.push(i + 1);
        f.push(i + 1);
    }
    let start = if n / sqrt == sqrt { 1 } else { 0 };
    for i in start..sqrt {
        x.push(n / (sqrt - i));
        f.push(n / (sqrt - i));
    }
    for i in 1..=sqrt as usize {
        let mut j = f.len() - i;
        let ii = f.len() - i;
        while j >= sqrt as usize && j >= i {
            f[ii] -= count[j] - count[sqrt as usize - 1];
            j -= i;
        }
    }
    let end = f.len();
    for i in (0..primes.len() as usize).rev() {
        if i as u64 % 1000 == 0 { dbg!(i); }
        let p = primes[i];
        sum = (sum + p % M * f[f.len() - p as usize] % M) % M;
        let mut j = f.len() - 1;
        while x[j] >= p as u64 {
            // find x[k] = j / p
            let d = x[j] / p as u64;
            let k = if d <= sqrt {
                d as usize - 1
            } else {
                end - (end - j) * p as usize
            };
            f[j] -= f[k];
            j -= 1;
        }
    }
    return (sum , (x, f));
}

fn main() {
    let start = Instant::now();

    // all number of form k * p where p is prime >= sqrt(n)
    let (n, (x, count, s)) = sum_primes(201820182018);
    let sqrt = (201820182018 as u64 as f64).sqrt() as usize;
    dbg!(n);
    let mut sum = 0;
    for i in 1..=sqrt {
        sum = (sum + s[s.len() - i] + M - s[sqrt - 1]) % M;
    }
    dbg!(sum);
    // from f, construct array without any k * p in it
    let primes = &EratosthenesSieve::new(sqrt).get_vector_of_primes();
    let s = sum_primes2(201820182018, &count, primes).0;
    dbg!(s);
    dbg!((sum + s) % M);

    let (n, (x, count, s)) = sum_primes(100);
    let sqrt = (100 as u64 as f64).sqrt() as usize;
    let mut sum = 0;
    for i in 1..=sqrt {
        sum = (sum + s[s.len() - i] + M - s[sqrt - 1]) % M;
    }
    dbg!(sum);
    // from f, construct array without any k * p in it
    let primes = &EratosthenesSieve::new(sqrt).get_vector_of_primes();
    let s = sum_primes2(100, &count, primes).0;
    dbg!(s);
    dbg!((sum + s) % M);

    let (n, (x, count, s)) = sum_primes(10000);
    let sqrt = (10000 as u64 as f64).sqrt() as usize;
    let mut sum = 0;
    for i in 1..=sqrt {
        sum = (sum + s[s.len() - i] + M - s[sqrt - 1]) % M;
    }
    dbg!(sum);
    // from f, construct array without any k * p in it
    let primes = &EratosthenesSieve::new(sqrt).get_vector_of_primes();
    let s = sum_primes2(10000, &count, primes).0;
    dbg!(s);
    dbg!((sum + s) % M);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
