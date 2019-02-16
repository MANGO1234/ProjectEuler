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

fn main() {
    let start = Instant::now();

    // for any a,b,c>=2, a^(b*c) is creative except when a=2,b=2,c=2

    let sieve = EratosthenesSieve::new(max(N_SQRT, 40) as usize);

    let mut comp_count = [0; 40];
    for i in 2..comp_count.len() {
        for j in 2..=i {
            if !sieve.is_prime(j as u64).unwrap() {
                comp_count[i]+=1;
            }
        }
    }

    let mut is_pow = [0; N_SQRT as usize + 1];
    for i in 2..is_pow.len() {
        if is_pow[i] == 0 {
            let mut k = i * i;
            while k < is_pow.len() {
                is_pow[k] = 1;
                k *= i;
            }
        }
    }


    let mut sum = 0;
    for i in 2..=N_SQRT {
        if is_pow[i as usize] == 1 {
            continue;
        }
        let is_prime = sieve.is_prime(i).unwrap();
        let mut pow = 1;
        let mut product : u64 = i;
        while product * i <= N {
            pow += 1;
            product *= i;
            // i is prime, need top power to be composite
            // i is composite and not a power, then (a*b)^c can transition to c^(a*b)
            if !is_prime || !sieve.is_prime(pow).unwrap() {
                sum += product;
            }
        }
    }
    sum-=16; // special case 2^2^2
    println!("{}", sum);

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
