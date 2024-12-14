use regex::Regex;
use std::io::{self};

fn parse() -> io::Result<String> {
    io::read_to_string(io::stdin())
}

pub fn do_a(input: &str) -> anyhow::Result<u32> {
    let re = Regex::new(r"mul\(([0-9]+),([0-9]+)\)").unwrap();

    re.captures_iter(input)
        .map(|c| {
            let (_, [x_str, y_str]) = c.extract();
            Ok(x_str.parse::<u32>()? * y_str.parse::<u32>()?)
        })
        .sum()
}

pub fn do_b(input: &str) -> anyhow::Result<u32> {
    let re = Regex::new(r"mul\((?<x>[0-9]+),(?<y>[0-9]+)\)|do\(\)|don't\(\)").unwrap();

    let result = re
        .captures_iter(input)
        .try_fold((true, 0), |(capturing, sum), c| {
            match c.get(0).unwrap().as_str() {
                "do()" => Ok::<_, anyhow::Error>((true, sum)),
                "don't()" => Ok((false, sum)),
                _ if !capturing => Ok((capturing, sum)),
                _ => {
                    let x = c.name("x").unwrap().as_str().parse::<u32>()?;
                    let y = c.name("y").unwrap().as_str().parse::<u32>()?;
                    Ok((capturing, sum + x * y))
                }
            }
        })?;
    Ok(result.1)
}

fn main() -> anyhow::Result<()> {
    let input = parse()?;
    let puzzle_a = do_a(&input)?;
    let puzzle_b = do_b(&input)?;
    Ok(println!("{}\n{}", puzzle_a, puzzle_b))
}
