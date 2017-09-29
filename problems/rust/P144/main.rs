mod euler;

use std::time::Instant;
#[allow(unused_imports)]
use euler::eratosthenes_sieve::EratosthenesSieve;

fn main() {
    let start = Instant::now();

    let mut i = 0;
    let mut x0 = 0.0;
    let mut y0 = 10.1;
    let mut x1 = 1.4;
    let mut y1 = -9.6;

    while x1 > 0.01 || x1 < -0.01 || y1 < 0.0 {
        let old_m = (y1 - y0) / (x1 - x0);
        let tangent = -4.0 * x1 / y1;

        // find m with reflection
        // https://stackoverflow.com/questions/17395860/how-to-reflect-a-line-over-another-line
        let m = (tangent * tangent * old_m + 2.0 * tangent - old_m) /
            (1.0 + 2.0 * tangent * old_m - tangent * tangent);
        let b = -m * x1 + y1;

        // simplify quadratic as much as possible and u get this equation
        let xn1 = (-b * m + (100.0 * m * m - 4.0 * b * b + 400f64).sqrt()) / (m * m + 4.0);
        let xn2 = (-b * m - (100.0 * m * m - 4.0 * b * b + 400f64).sqrt()) / (m * m + 4.0);
        let x2 = if (xn2 - x1).abs() > (xn1 - x1).abs() {
            xn2
        } else {
            xn1
        };
        let y2 = y1 + m * (x2 - x1);

        x0 = x1;
        x1 = x2;
        y0 = y1;
        y1 = y2;
        i += 1;
        println!("{} {} {}", i, x1, y1);
    }

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
