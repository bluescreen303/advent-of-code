use aoc_rs::year_2024::parse01;

fn main() -> anyhow::Result<()> {
    let (mut l, mut r) = parse01()?;

    l.sort();
    r.sort();

    let result = l
        .into_iter()
        .zip(r)
        .map(|(l, r)| (l as i32 - r as i32).abs() as u32)
        .fold(0, |acc, x| acc + x);
    Ok(println!("{}", result))
}
