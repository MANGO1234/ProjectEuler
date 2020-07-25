#![allow(unused_imports)]
mod euler;

use crate::euler::math_utils::{
    factorial_big, mod_inv, n_c_r, n_c_r_big, pow_mod, CombinationGenerator,
};
use core::fmt;
use euler::eratosthenes_sieve::EratosthenesSieve;
#[cfg(feature = "bigint")]
use num::ToPrimitive;
#[cfg(feature = "bigint")]
use num::{BigInt, BigRational, FromPrimitive};
use std::collections::{HashMap, HashSet, VecDeque};
use std::thread;
use std::time::Instant;

const N: usize = 7;

#[derive(Eq, PartialEq, Hash)]
struct Sig {
    sig: [i8; N],
    len: usize,
    count: i32,
}

impl fmt::Debug for Sig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "sig=[")?;
        for i in 0..N {
            // for i in 0..self.len {
            write!(f, "{:?}, ", self.sig[i])?;
        }
        write!(f, "] len={} count={}", self.len, self.count)
    }
}

fn merge(a: &Sig, b: &Sig) -> Sig {
    let mut ret = Sig {
        sig: [0; N],
        len: b.len,
        count: a.count,
    };

    // edges comp for bfs below
    let mut edges_a = [[false; N]; N];
    let mut edges_b = [[false; N]; N];
    for i in 0..a.len {
        if a.sig[i] != 0 {
            let idx = if a.sig[i] > 0 {
                i
            } else {
                -(a.sig[i] + 1) as usize
            };
            if b.sig[i] != 0 {
                edges_a[idx][i] = true;
                edges_b[i][idx] = true;
            }
            if a.len < b.len {
                if i + 1 < b.len && b.sig[i + 1] != 0 {
                    edges_a[idx][i + 1] = true;
                    edges_b[i + 1][idx] = true;
                }
            } else {
                if i > 0 && b.sig[i - 1] != 0 {
                    edges_a[idx][i - 1] = true;
                    edges_b[i - 1][idx] = true;
                }
            }
        }
    }

    // bfs to get connected components
    let mut a_visited = [false; N];
    let mut b_visited = [false; N];
    let mut queue = VecDeque::with_capacity(N * 2);
    for i in 0..b.sig.len() {
        if b.sig[i] == 0 {
            continue;
        }
        let mut count = 0;
        let mut set = [false; N];
        queue.clear();
        queue.push_back((1, i));
        while let Some((id, idx)) = queue.pop_front() {
            if id == 1 {
                if b_visited[idx] {
                    continue;
                }
                set[idx] = true;
                b_visited[idx] = true;
                count += 1;
                for i in 0..N {
                    if edges_b[idx][i] {
                        queue.push_back((0, i));
                    }
                }
            } else {
                if a_visited[idx] {
                    continue;
                }
                a_visited[idx] = true;
                count += a.sig[idx];
                for i in 0..N {
                    if edges_a[idx][i] {
                        queue.push_back((1, i));
                    }
                }
            }
        }

        let mut first = true;
        let mut first_idx = 0i8;
        for j in 0..set.len() {
            if set[j] {
                if first {
                    first = false;
                    first_idx = -(j as i8) - 1;
                    ret.sig[j] = count as i8;
                } else {
                    ret.sig[j] = first_idx;
                }
            }
        }
        ret.count = ret.count.max(count as i32);
    }
    ret
}

fn main() {
    let start = Instant::now();

    let mut poss = HashMap::new();
    poss.insert(
        Sig {
            sig: [0; N],
            len: 0,
            count: 0,
        },
        1u64,
    );
    let seed = vec![
        vec![0, 1],
        vec![0, 1],
        vec![0, 1],
        vec![0, 1],
        vec![0, 1],
        vec![0, 1],
        vec![0, 1],
        vec![0, 1],
    ];
    for i in 1..=N {
        let mut tmp = seed.clone();
        tmp.truncate(i);
        let mut gen = CombinationGenerator::new(tmp);
        let mut poss_new = HashMap::new();
        while let Some(comb) = gen.next() {
            let mut z = Sig {
                sig: [0; N],
                len: comb.len(),
                count: 0,
            };
            z.sig[0..comb.len()].copy_from_slice(comb.as_slice());
            for (p, &count) in &poss {
                let a = merge(p, &z);
                // dbg!(p, &z, &a);
                poss_new
                    .entry(a)
                    .and_modify(|e| *e += count)
                    .or_insert(count);
            }
        }
        poss = poss_new;
        dbg!(&poss.len());
    }
    for i in (1..N).rev() {
        let mut tmp = seed.clone();
        tmp.truncate(i);
        let mut gen = CombinationGenerator::new(tmp);
        let mut poss_new = HashMap::new();
        while let Some(comb) = gen.next() {
            let mut z = Sig {
                sig: [0; N],
                len: comb.len(),
                count: 0,
            };
            z.sig[0..comb.len()].copy_from_slice(comb.as_slice());
            for (p, &count) in &poss {
                let a = merge(p, &z);
                // dbg!(p, &z, &a);
                poss_new
                    .entry(a)
                    .and_modify(|e| *e += count)
                    .or_insert(count);
            }
        }
        poss = poss_new;
        dbg!(&poss.len());
    }
    let mut ans = 0.0;
    let mut dev = 1.0f64;
    for _ in 0..N * N {
        dev /= 2.0;
    }
    for (s, c) in poss {
        ans += s.count as f64 * dev * c as f64;
    }
    dbg!(ans);
    // dbg!(merge2(
    //     &Sig {
    //         sig: [(0, 1), (1, 1), (0, 0)],
    //         len: 2,
    //         count: 1
    //     },
    //     &Sig {
    //         sig: [(0, 1), (0, 1), (0, 0)],
    //         len: 3,
    //         count: 0,
    //     }
    // ));

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
