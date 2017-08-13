mod euler;

use std::time::Instant;
use euler::math_utils::*;

fn f2(top: u32, count: u32, total: u32, a: &mut [u32]) -> u64 {
    if count == 0 {
        let mut t = factorial(total);
        for i in 1..a.len() {
            if a[i as usize] > 1 {
                t /= factorial(a[i as usize]);
            }
        }
        return t;
    }

    let mut tot: u64 = 0;
    for i in (1..top + 1).rev() {
        a[i as usize] += 1;
        tot += f2(i, count - 1, total, a);
        a[i as usize] -= 1;
    }
    return tot;
}

fn f(n: u32, top: u32, count: u32, count2: u32, total: u32, a: &mut [u32]) -> u64 {
    if count == 0 {
        return if n == 0 { f2(top, count2, total, a) } else { 0 };
    }

    let mut tot: u64 = 0;
    for i in (1..top + 1).rev() {
        if i * count < n {
            break;
        } else if i > n {
            continue;
        }
        a[i as usize] += 1;
        tot += f(n - i, i, count - 1, count2, total, a);
        a[i as usize] -= 1;
    }
    return tot;
}

fn main() {
    let start = Instant::now();
    let mut a = [0 as u32; 13];
    println!("{:?}", f(70, 12, 10, 10, 20, &mut a));
    let end = Instant::now();
    let dur = end - start;
    println!("{:?}",
             dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000);
}
