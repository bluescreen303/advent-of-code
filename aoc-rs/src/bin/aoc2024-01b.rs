use std::collections::HashMap;

use aoc_rs::year_2024::parse01;

fn main() -> anyhow::Result<()> {
    let (l, r) = parse01()?;

    let mut counted = HashMap::new();
    for x in r {
        counted.entry(x).and_modify(|e| *e += 1).or_insert(1);
    }

    let result = l
        .into_iter()
        .map(|x| x * counted.get(&x).unwrap_or(&0))
        .fold(0, |acc, x| acc + x);
    Ok(println!("{}", result))
}
