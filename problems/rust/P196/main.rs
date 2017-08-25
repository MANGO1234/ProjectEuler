mod euler;

use std::time::Instant;
use euler::math_utils::*;
use euler::eratosthenes_sieve::EratosthenesSieve;
use std::f64::*;

fn segment(sieve: &mut EratosthenesSieve, t: &mut [u8], start: usize) {
    t[0] = 0;
    t[1] = 0;
    for p in sieve.as_vector_of_primes() {
        let mut i = ((p - start % p) % p) + 2;
        while i < t.len() {
            t[i] = 0;
            i += *p;
        }
    }
    let l = t.len() - 2;
    t[l] = 0;
    let l = t.len() - 1;
    t[l] = 0;
}

fn f(N: usize) -> u64 {
    let N2 = (((N + 3) * (N + 2) / 2) as f64).sqrt() as usize;
    let mut sieve = EratosthenesSieve::new(N2);
    let mut t = vec![1 as u8; (((N + 3) * (N + 2) - (N - 2) * (N - 3)) / 2) + 20];
    segment(
        &mut sieve,
        &mut t[0..(N - 2) + 4],
        (N - 3) * (N - 2) / 2 + 1,
    );
    segment(
        &mut sieve,
        &mut t[(N - 2) + 4..(2 * N - 3) + 8],
        (N - 2) * (N - 1) / 2 + 1,
    );
    segment(
        &mut sieve,
        &mut t[(2 * N - 3) + 8..(3 * N - 3) + 12],
        N * (N - 1) / 2 + 1,
    );
    segment(
        &mut sieve,
        &mut t[(3 * N - 3) + 12..(4 * N - 2) + 16],
        N * (N + 1) / 2 + 1,
    );
    segment(
        &mut sieve,
        &mut t[(4 * N - 2) + 16..5 * N + 20],
        (N + 1) * (N + 2) / 2 + 1,
    );
    let xs = [
        &t[0..(N - 2) + 4],
        &t[(N - 2) + 4..(2 * N - 3) + 8],
        &t[(2 * N - 3) + 8..(3 * N - 3) + 12],
        &t[(3 * N - 3) + 12..(4 * N - 2) + 16],
        &t[(4 * N - 2) + 16..5 * N + 20],
    ];
    let offset = N * (N - 1) / 2 + 1;
    let mut sum = 0;
    for i in 2..N + 2 {
        if xs[2][i] == 1 {
            let tot = xs[1][i - 1] + xs[1][i] + xs[1][i + 1] + xs[2][i - 1] + xs[2][i + 1] +
                xs[3][i - 1] + xs[3][i] + xs[3][i + 1];
            if tot >= 2 {
                sum += offset + i - 2;
            } else if tot == 1 {
                for j in -1 as i64..2 {
                    for k in -1 as i64..2 {
                        if j == 0 && k == 0 {
                            continue;
                        }
                        let ji = (2 + j) as usize;
                        let ki = (i as i64 + k) as usize;
                        if xs[ji][ki] != 1 {
                            continue;
                        }
                        let tot = xs[ji - 1][ki - 1] + xs[ji - 1][ki] + xs[ji - 1][ki + 1] +
                            xs[ji][ki - 1] + xs[ji][ki + 1] +
                            xs[ji + 1][ki - 1] +
                            xs[ji + 1][ki] + xs[ji + 1][ki + 1];
                        if tot >= 2 {
                            sum += offset + i - 2;
                        }
                    }
                }
            }
        }
    }
    return sum as u64;
}

fn main() {
    let start = Instant::now();

    println!("{}", f(5678027) + f(7208785));

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
