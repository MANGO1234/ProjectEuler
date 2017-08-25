mod euler;

use std::time::Instant;
use euler::math_utils::*;
use euler::eratosthenes_sieve::EratosthenesSieve;
use std::f64::*;
use std::cmp::max;

fn f_(arr: &[i64], a: usize, b: usize) -> (i64, i64, i64, i64) {
    if a + 1 == b {
        return (arr[a], arr[a], arr[a], arr[a]);
    }
    let m = (a + b) / 2;
    let (l1, m1, r1, s1) = f_(arr, a, m);
    let (l2, m2, r2, s2) = f_(arr, m, b);
    let mut newm = max(m1, m2);
    newm = max(newm, r1 + l2);
    let newl = max(l1, s1 + l2);
    let newr = max(r2, r1 + s2);
    return (newl, newm, newr, s1 + s2);
}

fn f(arr: &[i64]) -> i64 {
    let (l, m, r, _) = f_(arr, 0, arr.len());
    return max(max(l, m), r);
}

fn main() {
    let start = Instant::now();

    const N: usize = 2000;
    let mut s = vec![0 as i64; N * N];
    for k in 1 as i64..56 {
        s[(k - 1) as usize] = (100003 - 200003 * k + 300007 * k * k * k) % 1000000 - 500000;
    }
    for k in 55 as usize..4_000_000 {
        s[k] = (s[k - 24] + s[k - 55] + 1000000) % 1000000 - 500000;
    }

    let mut scratch = [0 as i64; N];
    let mut m: i64 = 0;
    for k in 0 as usize..N {
        m = max(m, f(&s[k * N..(k + 1) * N]));
    }
    for k in 0 as usize..N {
        let mut curr = k;
        for i in 0..N {
            scratch[i] = s[curr];
            curr += N;
        }
        m = max(m, f(&scratch[0..N]));
    }
    for k in 0 as usize..N {
        let mut curr = N * k;
        for i in 0..(N - k) {
            scratch[i] = s[curr];
            curr += N + 1;
        }
        m = max(m, f(&scratch[0..(N - k)]));
    }
    for k in 0 as usize..N - 1 {
        let mut curr = N - 1 - k;
        for i in 0..k + 1 {
            scratch[i] = s[curr];
            curr += N + 1;
        }
        m = max(m, f(&scratch[0..k + 1]));
    }
    for k in 0 as usize..N {
        let mut curr = N * (k + 1) - 1;
        for i in 0..(N - k) {
            scratch[i] = s[curr];
            curr += N - 1;
        }
        m = max(m, f(&scratch[0..(N - k)]));
    }
    for k in 0 as usize..N - 1 {
        let mut curr = k;
        for i in 0..k + 1 {
            scratch[i] = s[curr];
            curr += N - 1;
        }
        m = max(m, f(&scratch[0..k + 1]));
    }
    println!("{}", m);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
