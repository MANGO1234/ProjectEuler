mod euler;

use std::time::Instant;
use euler::math_utils::*;
use euler::eratosthenes_sieve::EratosthenesSieve;
use std::f64::*;

const N: i64 = 10;

fn f_h(sieve: &mut EratosthenesSieve, digit: i64, s: i32, curr: i64, i: usize) -> i64 {
    if i == N as usize {
        if sieve.do_division_test(curr as usize) {
            return curr;
        }
        return 0;
    }
    if (s >> i) & 1 == 0 {
        let mut sum = 0;
        for d in 0..10 {
            if d != digit && (i != 0 || d != 0) {
                sum += f_h(sieve, digit, s, curr * 10 + d, i + 1);
            }
        }
        return sum;
    } else {
        return f_h(sieve, digit, s, curr * 10 + digit, i + 1);
    }
}

fn f(sieve: &mut EratosthenesSieve, digit: i64, k: i64) -> i64 {
    //    let mut t = [-1; N as usize];
    let mut s = (1 << k) - 1;
    let mut sum = 0;
    while s & 1 << N == 0 {
        if digit != 0 || ((s & 1) != 1) {
            sum += f_h(sieve, digit, s, 0, 0);
        }
        // https://stackoverflow.com/questions/17435030/
        // how-to-find-all-possible-n-elements-subsets-of-a-set
        let lo = s & !(s - 1); // lowest one bit
        let lz = (s + lo) & !s; // lowest zero bit above lo
        s |= lz; // add lz to the set
        s &= !(lz - 1); // reset bits below lz
        s |= (lz / lo / 2) - 1; // put back right number of bits at end
    }
    return sum;
}

fn main() {
    let start = Instant::now();

    let mut sum = 0;
    let mut sieve = EratosthenesSieve::new(100000);
    for digit in 0..10 {
        for k in (1..N).rev() {
            let s = f(&mut sieve, digit, k);
            if s != 0 {
                println!("{} {} {}", digit, s, k);
                sum += s;
                break;
            }
        }
    }
    println!("{}", sum);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
