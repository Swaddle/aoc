use std::arch::aarch64::{vaddvq_f64, vdupq_n_f64, vfmaq_f64, vld1q_f64, vsetq_lane_f64};
use std::collections::HashMap;
use std::time::SystemTime;


// macro_rules! load_column_vector {
//     ($b:expr, $n:expr, $k:expr, $p:expr) => {{

//         let mut b_vec = vdupq_n_f64(0.0);
//         b_vec = vsetq_lane_f64::<0>(b0, b_vec);
//         b_vec = vsetq_lane_f64::<1>(b1, b_vec);
//         // b_vec = vsetq_lane_f64::<2>(b2, b_vec);
//         // b_vec = vsetq_lane_f64::<3>(b3, b_vec);
//         b_vec
//     }};
// }

// // blockwise SIMD matrix multiplication
// #[target_feature(enable = "neon")]
// pub unsafe fn matmul_simd(a: &Vec<u64>, b: &Vec<u64>, c: &mut Vec<u64>, n: usize) {
//     for j in 0..n {
//         for k in 0..n {

//             let mut sum = vdupq_n_u64(0.0);

//             for p in (0..n).step_by(2) {
//                 let a_vec = vld1q_u64(a.as_ptr().add(j * n + p));

//                 let b0 = *b.get_unchecked(p * n + k);
//                 let b1 = *b.get_unchecked((p + 1) * n + k);

//                 let b_vec = vdupq_n_u64(0.0);
//                 let b_vec = vsetq_lane_u64::<0>(b0, b_vec);
//                 let b_vec = vsetq_lane_u64::<1>(b1, b_vec);

//                 // add a_vec * b_vec to sum
//                 let

//                 //sum = vfmaq_f64(sum, a_vec, b_vec);
//             }

//             let result = vaddvq_f64(sum);
//             *c.get_unchecked_mut(j * n + k) = result;
//         }
//     }
// }

pub fn mat_by_vec_naive(a: &[u64], b: &[u64], c: &mut [u64], n: usize) {
    // matrix a is n x n
    // vector b is n x 1
    // vector c is n x 1

    for i in 0..n {
        for j in 0..n {
            c[i] += a[i * n + j] * b[j];
        }
    }
}

fn matmul_naive(a: &[u64], b: &[u64], c: &mut [u64], n: usize) {
    for i in 0..n {
        for j in 0..n {
            let mut sum = 0;
            for k in 0..n {
                sum += a[i * n + k] * b[k * n + j];
            }
            c[i * n + j] = sum;
        }
    }
}

pub fn matpow_naive_by_squaring(a: &[u64], b: &mut [u64], n: usize, mut p: usize) {
    // If p == 0, then b = I (the identity matrix)
    if p == 0 {
        for i in 0..n * n {
            b[i] = 0;
        }
        for i in 0..n {
            b[i * n + i] = 1;
        }
        return;
    }

    // result = I
    let mut result = vec![0u64; n * n];
    for i in 0..n {
        result[i * n + i] = 1;
    }

    // base = a
    let mut base = a.to_vec();
    let mut temp = vec![0u64; n * n];

    while p > 0 {
        if p % 2 == 1 {
            // result = result * base
            matmul_naive(&result, &base, &mut temp, n);
            result.copy_from_slice(&temp);
        }

        // base = base * base
        matmul_naive(&base, &base, &mut temp, n);
        base.copy_from_slice(&temp);

        p /= 2;
    }

    b.copy_from_slice(&result);
}

pub fn f(x: u64) -> Vec<u64> {
    // if x is 0
    if x == 0 {
        return vec![1];
    }
    // if x has an even number of digits,
    // split x into two equal parts,
    if x.to_string().len() % 2 == 0 {
        let s = x.to_string();
        let half = s.len() / 2;
        let (a, b) = s.split_at(half);
        return vec![a.parse::<u64>().unwrap(), b.parse::<u64>().unwrap()];
    }
    // otherwise multiply x by 2024
    return vec![x * 2024];
}

pub fn precompute_keys(x: u64, map: &mut HashMap<u64, Vec<u64>>) {
    map.insert(x, f(x));
    let mut queue = VecDeque::from(f(x));

    while let Some(key) = queue.pop_front() {
        if !map.contains_key(&key) {
            let values = f(key);
            map.insert(key, values.clone());
            queue.extend(values.into_iter().filter(|k| !map.contains_key(k)));
        }
    }
}

pub fn p1() -> u64 {
    let t0 = std::time::Instant::now();
    let input = std::fs::read_to_string("../data/11.txt").unwrap();

    // input is a list of integers separated by spaces

    let mut rule_map = HashMap::new();
    let total = input.len();


    for i in &input {
        precompute_keys(*i, &mut rule_map);
        println!("computed keys for key: {} out of {}", i, total);
    }

    // print length of keys 
    let keys = rule_map.keys().collect::<Vec<&u64>>();

    println!("total keys: {:?}", keys.len()); 

    let mut map = HashMap::new();
    precompute_keys(17, &mut map);

    let bind = map.clone();
    let keys = bind.keys().collect::<Vec<&u64>>();

    // println!("{:?}", keys);

    precompute_keys(125, &mut map);

    let keys_2 = map.keys().collect::<Vec<&u64>>();
    // println!("{:?}", keys_2);

    // join keys and keys_2, deduplicate, and sort
    let mut keys = keys
        .iter()
        .chain(keys_2.iter())
        .map(|x| **x)
        .collect::<Vec<u64>>();
    keys.sort();
    keys.dedup();
    // println!("{:?}", keys);

    // construct a tranisition_matrix
    // a number i produces a list of numbers j
    // stored in the map map[i] = [j1, j2, j3, ...]
    // j might have multiplicity
    // T[i][j] is the number of times i produces j,

    let n = keys.len();

    // Build a reverse lookup: key -> index
    let mut key_index = HashMap::with_capacity(n);
    for (i, &k) in keys.iter().enumerate() {
        key_index.insert(k, i);
    }

    let mut transition_matrix = vec![vec![0u64; n]; n];
    for (i, &key) in keys.iter().enumerate() {
        if let Some(values) = map.get(&key) {
            for value in values {
                // Use the precomputed index lookup
                if let Some(&j) = key_index.get(value) {
                    transition_matrix[i][j] += 1;
                }
            }
        }
    }
    let transition_matrix = transition_matrix
        .iter()
        .flatten()
        .cloned()
        .collect::<Vec<u64>>();
    let mut result = vec![0; n * n];
    matpow_naive_by_squaring(&transition_matrix, &mut result, 25, 0);


    let mut input_vec = vec![0; n];
    // find the index of 17 and 125 in keys
    let i = keys.iter().position(|x| *x == 17).unwrap();
    let j = keys.iter().position(|x| *x == 125).unwrap();
    input_vec[i] = 1;
    input_vec[j] = 1;

    // println!("{:?}", transition_matrix[n * j .. n * (j + 1)].iter().zip(keys.iter()).collect::<Vec<(&u64, &u64)>>());

    let mut output = vec![0; n];
    mat_by_vec_naive(&result, &input_vec, &mut output, n);
    // zip keys and output
    // println!("{:?}", keys.iter().zip(output.iter()).collect::<Vec<(&u64, &u64)>>());

    // convert into u64 vec
    let output = output.iter().map(|x| *x as u64).collect::<Vec<u64>>();

    //sum
    let sum = output.iter().sum::<u64>();

    println!("{:?}", sum);

    let t1 = t0.elapsed();
    println!("time: {:?}", t1);
    return 0;
}
