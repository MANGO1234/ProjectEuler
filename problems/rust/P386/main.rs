mod euler;

use std::time::Instant;
use euler::eratosthenes_sieve::EratosthenesSieve;

const N: u64 = 100_000_000;
const N_SQRT: u64 = 10_000;

fn tt(p_1: &Vec<u64>, i: usize, t: &mut [u64], mut acc: u64, tmp: &mut Vec<u64>) -> u64 {
    if i >= p_1.len() {
        return 0;
    }
    let accn = acc * p_1[i];
    if accn > N {
        return 0;
    }
    let mut new_tmp = tmp.clone();
    let mut sum = tt(p_1, i + 1, t, acc, &mut new_tmp);
    let mut k = 1;
    loop {
        acc = acc * p_1[i];
        if acc > N {
            break;
        }
        new_tmp.push(0);
        for i in 0..tmp.len() {
            new_tmp[i + k] += tmp[i];
        }
        sum += new_tmp[new_tmp.len() / 2];
        k += 1;
        if acc <= N_SQRT {
            t[acc as usize - 1] = new_tmp[new_tmp.len() / 2] + new_tmp[new_tmp.len() / 2 - 1];
        }
        sum += tt(p_1, i + 1, t, acc, &mut new_tmp);
    }
    return sum;
}

fn main() {
    let start = Instant::now();

    let sieve = EratosthenesSieve::new(N as usize);
    let primes = sieve.get_vector_of_primes();

    let p_1 = primes
        .iter()
        .cloned()
        .take_while(|&x| x <= N_SQRT)
        .collect();
    let mut tmp = Vec::with_capacity(20);
    tmp.push(1);
    let mut t = [0; N_SQRT as usize];
    t[0] = 1;
    let mut sum = 1 + tt(&p_1, 0, &mut t, 1, &mut tmp);

    let p_2: Vec<u64> = primes
        .iter()
        .cloned()
        .skip_while(|&x| x <= N_SQRT)
        .collect();
    for (i, _) in t.iter().enumerate() {
        let upper = match p_2.binary_search(&(N / (i as u64 + 1))) {
            Ok(k) => k + 1,
            Err(k) => k,
        } as u64;
        sum += t[i] * upper;
    }
    println!("{}", sum);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
