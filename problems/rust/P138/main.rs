mod euler;

use std::time::Instant;
use euler::math_utils::*;

fn main() {
    let start = Instant::now();

    // generates solutions to (m^2-n^2)-2*(2*m*n)=+-1
    let mut sum: i64 = 0;
    let p: i64 = 17;
    let q: i64 = 4;
    let r: i64 = 4;
    let s: i64 = 1;
    let mut x: i64 = 1;
    let mut y: i64 = 0;
    for i in 0..6 {
        let tmpx = p * x + q * y;
        y = r * x + s * y;
        x = tmpx;
        sum += x * x + y * y;
    }
    let mut x: i64 = 4;
    let mut y: i64 = 1;
    for i in 0..5 {
        let tmpx = p * x + q * y;
        y = r * x + s * y;
        x = tmpx;
        sum += x * x + y * y;
    }
    sum += 17;
    println!("{}", sum);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
