mod euler;

use std::time::Instant;
use euler::eratosthenes_sieve::EratosthenesSieve;

fn g(pows: &Vec<u64>, prefix: &Vec<u64>, singles: &Vec<u64>, i: usize, acc: u64) -> u64 {
    if i >= singles.len() {
        return 0;
    }
    let mut sum = 0;
    if singles[i] <= 1_000_000_000_000 / acc {
        sum += match pows.binary_search(&(1_000_000_000_000 / (acc * singles[i]))) {
            Ok(k) => acc * singles[i] * prefix[k],
            Err(k) => acc * singles[i] * prefix[k - 1],
        };
        sum += g(pows, prefix, singles, i + 1, acc);
        sum += g(pows, prefix, singles, i + 1, acc * singles[i]);
    }
    return sum;
}

fn main() {
    let start = Instant::now();

    let mut sieve = EratosthenesSieve::new(1_000_000);

    let mut pows = Vec::new();
    let mut k: u64 = 1;
    while k < 1_000_000_000_000 {
        let oldk = k;
        while k < 1_000_000_000_000 {
            let oldk = k;
            while k < 1_000_000_000_000 {
                pows.push(k);
                k *= 5;
            }
            k = oldk * 3;
        }
        k = oldk * 2;
    }

    pows.sort();
    let singles: Vec<u64> = pows.iter()
        .skip(5)
        .map(|x| x + 1)
        .filter(|x| sieve.do_division_test(*x))
        .collect();
    pows.push(1_000_000_000_000);
    let mut prefix = Vec::with_capacity(pows.len());
    let mut s = 0;
    for &x in &pows {
        s += x;
        prefix.push(s);
    }

    println!(
        "{:?}",
        (prefix[prefix.len() - 1] + g(&pows, &prefix, &singles, 0, 1)) % (1 << 32)
    );

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
