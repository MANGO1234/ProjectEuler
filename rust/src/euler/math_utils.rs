use crate::EratosthenesSieve;
use num::integer::Roots;
#[cfg(feature = "bigint")]
use num::BigInt;
use once_cell::sync::Lazy;
use std::sync::{Mutex, MutexGuard};

const FACTORIALS: [u64; 21] = [
    1,
    1,
    2,
    6,
    24,
    120,
    720,
    5040,
    40320,
    362880,
    3628800,
    39916800,
    479001600,
    6227020800,
    87178291200,
    1307674368000,
    20922789888000,
    355687428096000,
    6402373705728000,
    121645100408832000,
    2432902008176640000,
];

pub fn factorial(n: u32) -> u64 {
    if n > 20 {
        return 0;
    }
    return FACTORIALS[n as usize];
}

#[cfg(feature = "bigint")]
pub fn factorial_big(n: u64) -> BigInt {
    if n <= 20 {
        return BigInt::from(FACTORIALS[n as usize]);
    }
    let mut t = BigInt::from(FACTORIALS[20]);
    for i in 21..=n {
        t *= i;
    }
    t
}

pub fn n_c_r(mut n: u64, r: u64) -> u64 {
    let mut a = 1;
    for i in 1..=r {
        a *= n;
        a /= i;
        n -= 1;
    }
    a
}

#[cfg(feature = "bigint")]
pub fn n_c_r_big(mut n: u64, r: u64) -> BigInt {
    let mut a = BigInt::from(1u64);
    for i in 1..=r {
        a *= n;
        a /= i;
        n -= 1;
    }
    a
}

pub fn gcd(a: u64, b: u64) -> u64 {
    // using Euclidean algorithm
    let mut a = a;
    let mut b = b;
    while b != 0 {
        let temp = a;
        a = b;
        b = temp % a;
    }
    return a;
}

// keep here for modification according to problem
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

/**
 * Return n^pow mod m using fast exponentiation. Doesn't do any overflow check.
 *
 * @param n
 * @param pow
 * @param m
 * @return n^pow mod m
 */
pub fn pow_mod(n: u32, pow: u64, m: u32) -> u64 {
    let m = m as u64;
    let mut ans = 1u64;
    let mut temp = n as u64;
    let mut pow = pow;
    while pow != 0 {
        if pow & 1 == 1 {
            ans = (ans * temp) % m;
        }
        temp = temp * temp % m;
        pow >>= 1;
    }
    return ans % m;
}

pub fn mod_inv(n: u32, m: u32) -> u64 {
    return pow_mod(n, m as u64 - 2, m);
}

pub struct CombinationGenerator<T> {
    a: Vec<T>,
    next: Vec<usize>,
    done_iter: bool,
    seed: Vec<Vec<T>>,
}

impl<T: Copy> CombinationGenerator<T> {
    pub fn new(seed: Vec<Vec<T>>) -> CombinationGenerator<T> {
        CombinationGenerator {
            a: seed.iter().map(|x| x[0]).collect(),
            next: vec![0; seed.len()],
            done_iter: false,
            seed,
        }
    }

    pub fn reset(&mut self) {
        self.done_iter = false;
        for i in 0..self.seed.len() {
            self.a[i] = self.seed[i][0];
        }
    }

    pub fn next(&mut self) -> Option<&Vec<T>> {
        if self.done_iter {
            return None;
        }
        if self.next[0] < self.seed[0].len() {
            self.a[0] = self.seed[0][self.next[0]];
            self.next[0] += 1;
            return Some(&self.a);
        }
        self.next[0] = 1;
        self.a[0] = self.seed[0][0];
        let mut start = 1;
        loop {
            if start >= self.seed.len() {
                self.done_iter = true;
                return None;
            }
            self.next[start] += 1;
            if self.next[start] >= self.seed[start].len() {
                self.next[start] = 0;
                self.a[start] = self.seed[start][0];
            } else {
                self.a[start] = self.seed[start][self.next[start]];
                break;
            }
            start += 1;
        }
        return Some(&self.a);
    }
}

static PRIME_SEARCHER: Lazy<Mutex<EratosthenesSieve>> =
    Lazy::new(|| Mutex::new(EratosthenesSieve::new(100)));

fn make_sure_there_is_enough_prime_to_do_primality_test(
    x: &mut MutexGuard<EratosthenesSieve>,
    num: u64,
) {
    if num as usize > x.size * x.size {
        x.expand_to_size(num.sqrt() as usize + 1);
    }
}

/**
 * Returns all the unique prime factors of a number in an integer array, sorted.
 *
 * Example: 480 -> [2, 3, 5] even though it's prime factorization is 2^5 * 3 * 5
 * @param num
 * @return a sorted integer array containing all the prime factors of num
 */
pub fn unique_prime_factors_of(mut num: u64) -> Vec<u64> {
    //if it's within range of prime_searcher and is a prime (so as not to trigger an expensive call from
    //.isPrime() or smaller than 2
    let mut prime_searcher = PRIME_SEARCHER.lock().unwrap();
    if num < prime_searcher.size as u64 && prime_searcher.is_prime_unsafe(num) {
        return vec![num];
    } else if num < 2 {
        return vec![];
    }

    make_sure_there_is_enough_prime_to_do_primality_test(&mut prime_searcher, num);

    let mut factors = Vec::new();
    let mut i = 0usize; //keep track of primes index
    let primes = prime_searcher.as_vector_of_primes();
    while num != 1 && i < primes.len() {
        let temp_prime = primes[i];
        i += 1;
        if num % temp_prime == 0 {
            factors.push(temp_prime);
            num /= temp_prime;
            //loop through until there is no more multiples of this prime factor
            while num % temp_prime == 0 {
                num /= temp_prime;
            }
        }
    }
    if num != 1 {
        factors.push(num);
    }
    return factors;
}
