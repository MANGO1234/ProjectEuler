#![allow(unused_imports)]
mod euler;

use crate::euler::math_utils::{mod_inv, n_c_r, pow_mod, CombinationGenerator};
use core::fmt;
use euler::eratosthenes_sieve::EratosthenesSieve;
use itertools::Itertools;
#[cfg(feature = "bigint")]
use num::ToPrimitive;
#[cfg(feature = "bigint")]
use num::{BigInt, BigRational, FromPrimitive};
use std::collections::{HashMap, HashSet, VecDeque};
use std::thread;
use std::time::Instant;

const N: usize = 20;
const M: u64 = 1001001011;

fn tag_component_h(
    v: &Vec<Vec<usize>>,
    tags: &mut Vec<i32>,
    i: usize,
    last: usize,
    tag: i32,
) -> bool {
    if tags[i] != 0 {
        if tags[i] > tag {
            return false;
        } else if tags[i] == tag {
            return true;
        }
    }
    tags[i] = tag;
    let mut cycle = false;
    for &next in &v[i] {
        if next != last {
            cycle |= tag_component_h(v, tags, next, i, tag);
        }
    }
    cycle
}

fn tag_component(v: &Vec<Vec<usize>>, tags: &mut Vec<i32>, i: usize, tag: i32) -> bool {
    tag_component_h(v, tags, i, v.len(), tag)
}

fn count_tree(v: &Vec<Vec<usize>>, i: usize, last: usize) -> (u64, u64) {
    let mut tmp = Vec::new();
    for &next in &v[i] {
        if next != last {
            tmp.push(count_tree(v, next, i));
        }
    }
    (
        tmp.iter().map(|(a, b)| a + b).fold(1, |a, x| a * x % M),
        tmp.iter().map(|(a, _)| a).fold(1, |a, x| a * x % M),
    )
}

fn count_cycle_h(
    v: &Vec<Vec<usize>>,
    i: usize,
    last: usize,
    first: usize,
    first1: bool,
) -> (u64, u64) {
    // dbg!(i, first, last);
    if last != first && v[i].contains(&first) {
        return if first1 {
            if v[i].len() == 3 {
                (2, 0)
            } else {
                (1, 0)
            }
        } else {
            if v[i].len() == 3 {
                (2, 1)
            } else {
                (1, 1)
            }
        };
    }
    if v[i].len() == 1 {
        return (1, 1);
    }
    let mut tmp = Vec::new();
    for &next in &v[i] {
        if next != last {
            tmp.push(count_cycle_h(v, next, i, first, first1));
        }
    }
    (
        tmp.iter().map(|(a, b)| a + b).fold(1, |a, x| a * x % M),
        tmp.iter().map(|(a, _)| a).fold(1, |a, x| a * x % M),
    )
}

fn count_cycle(v: &Vec<Vec<usize>>, mut i: usize) -> (u64, u64) {
    let last = i;
    if v[i].len() == 1 {
        // this is not proven, at the N given all node immediately lead into a cycle
        i = v[i][0];
    }
    if v[i].len() == 2 {
        let (a, b) = count_cycle_h(v, v[i][0], i, i, false);
        let (a2, _) = count_cycle_h(v, v[i][0], i, i, true);
        return ((a + b) % M, a2);
    } else if v[i].len() == 3 {
        let next = if v[i][0] == last { v[i][1] } else { v[i][0] };
        let (a, b) = count_cycle_h(v, next, i, i, false);
        let (a2, _) = count_cycle_h(v, next, i, i, true);
        return ((a + b) * 2 % M, a2);
    }
    panic!()
}

fn main() {
    let start = Instant::now();

    let mut f = vec![1u64, 2u64];
    for i in 2..(1 << 6) {
        f.push(f[i - 1] + f[i - 2]);
    }
    // special case f[1], could also move everything up by 1, but then numbers don't match
    f[1] = 1;
    // P209 -> used graphviz, I solved a long time ago, resolved it without realizin
    dbg!(f[2] * (f[2] + f[0]) * (f[5] + f[3]) * (f[5] + f[3]) * (f[45] + f[43]));

    let mut graph = vec![Vec::new(); 1 << N];
    // skip 0 because we know 0 -> 0, which is forced to be 0
    for n in 1..(1 << N) {
        let mut t = [0; 1 << N];
        for i in 0..N {
            t[i] = (n >> i) & 1;
        }
        let mut next = (t[0] & (t[1] ^ t[2])) << (N - 1);
        for i in 1..N {
            next |= t[i] << (i - 1);
        }
        if !graph[n].contains(&next) && next > 0 {
            graph[n].push(next);
            graph[next].push(n);
        }
    }

    let mut tags = vec![0; 1 << N];
    let mut cycles = HashSet::new();
    let mut count = 1;
    for i in 1..(1 << N) {
        if tags[i] == 0 {
            if tag_component(&graph, &mut tags, i, i as i32) {
                let (a, b) = count_cycle(&graph, i);
                count = (count * (a + b)) % M;
                cycles.insert(i);
            } else {
                let (a, b) = count_tree(&graph, i, graph.len());
                count = (count * (a + b)) % M;
            }
        }
    }
    // for (i, v) in tags.iter().enumerate() {
    //     if cycles.contains(v) {
    //         for edge in &graph[i] {
    //             println!("{} -> {};", i, edge);
    //         }
    //     }
    // }
    dbg!(count);

    let end = Instant::now();
    let dur = end - start;
    println!(
        "{:?}",
        dur.as_secs() * 1000 + dur.subsec_nanos() as u64 / 1000000
    );
}
