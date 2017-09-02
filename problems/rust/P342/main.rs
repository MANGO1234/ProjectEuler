mod euler;

use std::time::Instant;
use euler::eratosthenes_sieve::EratosthenesSieve;
use std::thread::Builder;

const TOTAL: u64 = 10_000_000_000;

struct Factor {
    pub prime: u64,
    pub count: usize,
    pub add: Vec<usize>,
    pub new_prime: bool,
}

impl Factor {
    pub fn new(prime: u64, add: Vec<usize>) -> Factor {
        return Factor {
            prime: prime,
            add: add,
            new_prime: true,
            count: 0,
        };
    }
}

fn f(factors: &mut Vec<Factor>, i: usize, acc: u64) -> u64 {
    let mut sum = 0;
    if factors[i].new_prime {
        let mut newacc = acc * factors[i].prime;
        let mut count = factors[i].count + 1;
        while count % 3 != 0 && newacc < TOTAL {
            newacc *= factors[i].prime;
            count += 2;
        }
        if newacc < TOTAL {
            {
                let (left, right) = factors.split_at_mut(i);
                for &j in &right[0].add {
                    left[j].count += 1;
                }
            }
            factors[i].new_prime = false;
            sum += f(factors, i, newacc);
            factors[i].new_prime = true;
            {
                let (left, right) = factors.split_at_mut(i);
                for &j in &right[0].add {
                    left[j].count -= 1;
                }
            }
        }
        if factors[i].count % 3 == 0 {
            if i == 0 {
                if acc != 1 {
                    sum += acc;
                }
            } else {
                sum += f(factors, i - 1, acc);
            }
        }
    } else {
        let mut newacc = acc * factors[i].prime;
        if newacc < TOTAL {
            newacc *= factors[i].prime;
        }
        if newacc < TOTAL {
            newacc *= factors[i].prime;
        }
        if newacc < TOTAL {
            sum += f(factors, i, newacc);
        }
        if i == 0 {
            if acc != 1 {
                sum += acc;
            }
        } else {
            sum += f(factors, i - 1, acc);
        }
    }
    return sum;
}

fn main() {
    let start = Instant::now();

    // stack overflow, spawn a thread with larger stack size
    let builder = Builder::new().stack_size(32 * 1024 * 1024);
    let handler = builder
        .spawn(|| {
            let sieve = EratosthenesSieve::new(100001);
            let primes = sieve.get_vector_of_primes();
            let mut factors = Vec::with_capacity(primes.len());
            for i in 0..primes.len() {
                let mut add = Vec::with_capacity(20);
                let mut k = primes[i] - 1;
                for j in 0..i {
                    while k % primes[j] == 0 {
                        k /= primes[j];
                        add.push(j);
                    }
                    if k == 1 {
                        break;
                    }
                }
                factors.push(Factor::new(primes[i], add))
            }
            let ans = f(&mut factors, primes.len() - 1, 1);
            println!("{}", ans);
        })
        .unwrap();
    handler.join().unwrap();

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
