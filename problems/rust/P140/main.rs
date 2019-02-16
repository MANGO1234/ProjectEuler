mod euler;

#[allow(unused_imports)]
use euler::eratosthenes_sieve::EratosthenesSieve;
use std::time::Instant;
use std::fmt;
use std::cmp::max;

const N_SQRT : u64 = 1000000;
const N : u64 = N_SQRT * N_SQRT;

struct Array<T> {
    data: [T; N_SQRT as usize]
}

// generating function -> quadratic formula -> rational -> solve 5x^2+14x+1=y^2, x and y integer
// using https://www.alpertron.com.ar/QUAD.HTM

fn f1(vec: &mut Vec<i64>, mut x: i64,mut y:i64) {
    for _ in 0..14 {
        let old_x = x;
        let old_y = y;
        x = -9 * old_x - 4 * old_y - 14;
        y = -20 * old_x - 9 * old_y - 28;
        if x > 0 && y.abs() > x + 1 {
            vec.push(x);
        }
    }
}

fn f2(vec: &mut Vec<i64>, mut x: i64,mut y:i64) {
    for _ in 0..14 {
        let old_x = x;
        let old_y = y;
        x = -9 * old_x + 4 * old_y - 14;
        y = 20 * old_x - 9 * old_y + 28;
        if x > 0 && y.abs() > x + 1 {
            vec.push(x);
        }
    }
}

fn main() {
    let start = Instant::now();

    let mut vec = Vec::new();

    f1(&mut vec,2, -7);
    f1(&mut vec, 0, -1);
    f1(&mut vec, 0, 1);
    f1(&mut vec, -4, 5);
    f1(&mut vec, -3, 2);
    f1(&mut vec, -3, -2);

    f2(&mut vec, 2, -7);
    f2(&mut vec, 0, -1);
    f2(&mut vec, 0, 1);
    f2(&mut vec, -4, 5);
    f2(&mut vec, -3, 2);
    f2(&mut vec, -3, -2);

    vec.sort();
    vec.dedup_by_key(|&mut n| n);
    dbg!(vec[19]);
    let mut sum = 0;
    for i in 0..30 {
        sum+=vec[i];
    }
    dbg!(sum);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}

impl<T: fmt::Debug> fmt::Debug for Array<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.data[..].fmt(formatter)
    }
}
