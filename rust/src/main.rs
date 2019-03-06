#![allow(unused_imports)]
mod euler;

use euler::eratosthenes_sieve::EratosthenesSieve;
use euler::math_utils::gcd;
use std::cmp::max;
use std::thread;
use std::time::Instant;

const M: u64 = 1000000000;

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
    let mut lcm = 1;
    let mut next_lcm = 1;
    let mut count = 0;
    for i in 1..=31 {
        let n = 1 << (2 * i);
        lcm = lcm / gcd(lcm, i) * i;
        next_lcm = next_lcm / gcd(next_lcm, i + 1) * (i + 1);
        // (n - 1) / lcm + 1 give the correct count, but need to skip 1 (don't add 1) and N (subtract 2)
        count += (n - 2) / lcm - (n - 2) / next_lcm;
    }
    dbg!(count);

    let mut count_even = 0;
    let mut count_odd = 0;
    for n in 1..=177 {
        let k: u64 = n + 1;
        let mut t = (k * k) * (k + 1) * (k + 1) / 4 - k * k;
        let t2 = t;
        let mut count = 0;
        while t % 2 == 0 {
            count += 1;
            t /= 2;
        }
        if k % 2 == 0 {
            count_even += count
        } else {
            count_odd += count
        };
        println!(
            "{} {} {} {}",
            k,
            if n < 13 { 6u64.pow(n as u32) } else { 0 },
            t2,
            count
        );
    }
    dbg!(count_even);
    dbg!(count_odd);

    let mut count = 0;
    for i in 2..=53 {
        count += (100 + 1) / (1u64 << i) * 2;
    }
    dbg!(count);

    let mut count = 0;
    for i in 2..=53 {
        count += (100 + 1) / (1u64 << i) * 2;
    }
    dbg!(count);

    // let k = n + 1, then E(m, n)=(k*(k+1)/2)^m-k^m, then just consider odd and even to get
    // k is even with j 2 factors, then there is (j - 1) * m
    // k is odd with form 4j+1, then ((4j+1)+1)^m-1=>2j+1
    const N: u64 = 10000000000000000;
    let mut count = 0;
    for i in 2..=53 {
        count += (N + 1) / (1 << i);
    }
    dbg!(count);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
