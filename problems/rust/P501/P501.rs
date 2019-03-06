#![allow(unused_imports)]
mod euler;

use euler::eratosthenes_sieve::EratosthenesSieve;
use std::cmp::max;
use std::thread;
use std::time::Instant;

// f(x,p_i)=f(x,p_(i-1))+f(x/p_i, p_(i-1))
// f[x.binary_search(&k).unwrap()] to find number of primes below k
// where k <= floor(sqrt(n)) or k = floor(n/m) where m<=sqrt(n)
fn count_primes(n: u64) -> (u64, (Vec<u64>, Vec<u64>)) {
    let sqrt = (n as f64).sqrt() as u64;
    let mut x = Vec::with_capacity(sqrt as usize * 2);
    let mut f = Vec::with_capacity(sqrt as usize * 2);
    for i in 0..sqrt {
        x.push(i + 1);
        f.push(i);
    }
    let start = if n / sqrt == sqrt { 1 } else { 0 };
    for i in start..sqrt {
        x.push(n / (sqrt - i));
        f.push(n / (sqrt - i) - 1);
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
                f[j] -= f[k] - f[i - 1];
                j -= 1;
            }
        }
    }
    return (f[f.len() - 1], (x, f));
}

const N: u64 = 1000000000000;

fn main() {
    let start = Instant::now();

    let (n, (x, f)) = count_primes(N);
    dbg!(n);
    let mut count = 15; // 7th power below 10^12
    let mut sieve = EratosthenesSieve::new(10000);
    let primes = sieve.as_vector_of_primes();
    for p in primes {
        // find a^4*b^2
        count += f[x.binary_search(&(N / p / p / p)).unwrap()];
        if N / p / p / p > *p {
            count -= 1;
        }
    }
    let mut sieve = EratosthenesSieve::new(1000000);
    let primes = sieve.as_vector_of_primes();
    let mut i = 0;
    while i < primes.len() {
        // find a^2*b^2*c^2
        let mut j = i + 1;
        while j < primes.len() {
            if N / primes[i] / primes[j] < primes[j] {
                break;
            }
            count += f[x.binary_search(&(N / (primes[i] * primes[j]))).unwrap()]
                - f[x.binary_search(&primes[j]).unwrap()];
            j += 1;
        }
        i += 1;
    }
    dbg!(count);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
