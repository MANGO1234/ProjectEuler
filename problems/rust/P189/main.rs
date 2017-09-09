mod euler;

use std::time::Instant;
#[allow(unused_imports)]
use euler::eratosthenes_sieve::EratosthenesSieve;

fn g_up(k: u64, n: u32, acc: usize, j: usize, up: &mut Vec<u64>) {
    if n == 0 {
        if j & 3 != 0 {
            up[(acc << 2) + 0] += k;
        }
        if j & 3 != 1 {
            up[(acc << 2) + 1] += k;
        }
        if j & 3 != 2 {
            up[(acc << 2) + 2] += k;
        }
        return;
    }
    if j & 3 != 0 && j & (3 << 2) != (0 << 2) {
        g_up(k, n - 1, (acc << 2) + 0, j >> 2, up);
    }
    if j & 3 != 1 && j & (3 << 2) != (1 << 2) {
        g_up(k, n - 1, (acc << 2) + 1, j >> 2, up);
    }
    if j & 3 != 2 && j & (3 << 2) != (2 << 2) {
        g_up(k, n - 1, (acc << 2) + 2, j >> 2, up);
    }
}

fn g_down(k: u64, n: u32, acc: usize, j: usize, down: &mut Vec<u64>) {
    if n == 0 {
        down[acc] += k;
        return;
    }
    if j & 3 != 0 {
        g_down(k, n - 1, (acc << 2) + 0, j >> 2, down);
    }
    if j & 3 != 1 {
        g_down(k, n - 1, (acc << 2) + 1, j >> 2, down);
    }
    if j & 3 != 2 {
        g_down(k, n - 1, (acc << 2) + 2, j >> 2, down);
    }
}

fn main() {
    let start = Instant::now();

    // u32: every 2 bits is a triangle in row 0 red, 1 green, 2 red, 3 unknown (used in g_up)
    let mut up: Vec<u64> = vec![1, 1, 1, 0];
    for i in 1..8 {
        let mut down = vec![0u64; 3 * 4i32.pow(i - 1) as usize];
        let mut newup = vec![0u64; 3 * 4i32.pow(i) as usize];
        for j in 0..up.len() {
            if up[j] != 0 {
                g_down(up[j], i, 0, j, &mut down);
            }
        }
        for j in 0..down.len() {
            if down[j] != 0 {
                g_up(down[j], i, 0, (j << 2) + 3, &mut newup);
            }
        }
        up = newup;
    }
    println!("{}", up.iter().sum::<u64>());

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
