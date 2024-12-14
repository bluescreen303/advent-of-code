use anyhow::anyhow;
use std::collections::HashMap;
use std::io;

pub fn parse() -> anyhow::Result<(Vec<u32>, Vec<u32>)> {
    let mut buffer = String::new();
    let mut left = vec![];
    let mut right = vec![];
    while io::stdin().read_line(&mut buffer)? > 0 {
        let mut s = buffer.split_ascii_whitespace();
        left.push(s.next().ok_or(anyhow!("left column not found"))?.parse()?);
        right.push(s.next().ok_or(anyhow!("right column not found"))?.parse()?);
        buffer.clear();
    }
    Ok((left, right))
}

fn main() -> anyhow::Result<()> {
    let (mut l, mut r) = parse()?;

    l.sort();
    r.sort();

    let puzzle_a = l
        .iter()
        .zip(&r)
        .map(|(l, r)| (*l as i32 - *r as i32).unsigned_abs())
        .sum::<u32>();

    let mut counted = HashMap::new();
    for x in r {
        counted.entry(x).and_modify(|e| *e += 1).or_insert(1);
    }

    let puzzle_b = l
        .into_iter()
        .map(|x| x * counted.get(&x).unwrap_or(&0))
        .sum::<u32>();
    Ok(println!("{}\n{}", puzzle_a, puzzle_b))
}
