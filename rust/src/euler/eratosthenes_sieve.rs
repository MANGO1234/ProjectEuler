#[allow(dead_code)]
pub struct EratosthenesSieve {
    sieve: Vec<bool>,
    size: usize,
    num_of_primes: usize,
    primes_vec: Vec<usize>,
}

#[allow(dead_code)]
impl EratosthenesSieve {
    pub fn new(mut size: usize) -> EratosthenesSieve {
        if size < 10 {
            size = 10;
        }
        let (arr, num_of_primes) = EratosthenesSieve::create_sieve_of_size(size);
        return EratosthenesSieve {
            sieve: arr,
            size: size,
            num_of_primes: num_of_primes,
            primes_vec: vec![],
        };
    }

    pub fn is_prime(&self, n: usize) -> Option<bool> {
        if n % 2 == 0 {
            return Option::Some(n == 2);
        } else if n < 2 {
            return Option::Some(false);
        }
        if ((n / 2 - 1) as usize) < self.sieve.len() {
            return Option::Some(self.sieve[(n / 2 - 1) as usize]);
        }
        return Option::None;
    }

    pub fn is_prime_unsafe(&self, n: usize) -> bool {
        if n % 2 == 0 {
            return n == 2;
        } else if n > 2 {
            return self.sieve[n / 2 - 1];
        }
        return false;
    }

    pub fn as_vector_of_primes(&mut self) -> &Vec<usize> {
        if self.primes_vec.len() == 0 {
            self.primes_vec = Vec::with_capacity(self.num_of_primes as usize);
            self.primes_vec.push(2);
            for (i, &b) in self.sieve.iter().enumerate() {
                if b {
                    self.primes_vec.push((i * 2 + 3));
                }
            }
        }
        return &self.primes_vec;
    }

    pub fn expand_to_size(&mut self, size: usize) -> bool {
        if size < self.size {
            return false;
        }
        let (arr, num_of_primes) = EratosthenesSieve::create_sieve_of_size(size);
        self.sieve = arr;
        self.num_of_primes = num_of_primes;
        self.size = size;
        self.primes_vec = vec![];
        return true;
    }

    fn create_sieve_of_size(size: usize) -> (Vec<bool>, usize) {
        let len = ((size + 1) / 2 - 1) as usize;
        let bound = (size as f64).sqrt() as usize / 2;
        let mut sieve = vec![true; len as usize];

        for i in 0..bound {
            if sieve[i] {
                let skip = i * 2 + 3;
                let mut j = (skip * skip - 3) / 2;
                while j < len {
                    sieve[j] = false;
                    j += skip;
                }
            }
        }

        let count = 1 + sieve.iter().filter(|&&x| x).count();
        return (sieve, count);
    }

    pub fn do_division_test(&mut self, n: usize) -> bool {
        if n <= self.size {
            return self.is_prime_unsafe(n);
        }
        if n as usize > self.size * self.size {
            return false;
        }
        for p in self.as_vector_of_primes() {
            if n as usize % p == 0 {
                if n == 1117 {
                    print!("ASSADASD {}", p);
                }
                return false;
            }
        }
        return true;
    }
}
