use anyhow::anyhow;
use aoc_rs::ParseFrom;
use regex::Regex;
use std::{io, str::FromStr, sync::LazyLock};

#[derive(Debug)]
struct Puzzle {
    ax: i64,
    ay: i64,
    bx: i64,
    by: i64,
    px: i64,
    py: i64,
}

impl Puzzle {
    fn presses(&self) -> Option<(i64, i64)> {
        let a = ((self.py * self.bx) - (self.px * self.by))
            / ((self.ay * self.bx) - (self.ax * self.by));
        let b = (self.px - (a * self.ax)) / self.bx;
        if a * self.ax + b * self.bx == self.px && a * self.ay + b * self.by == self.py {
            Some((a, b))
        } else {
            None
        }
    }

    fn score(&self) -> Option<i64> {
        self.presses().map(|(a, b)| a * 3 + b)
    }

    fn for_puzzle_b(&mut self) {
        self.px += 10000000000000;
        self.py += 10000000000000;
    }
}

static PUZZLE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"Button A:\s+X\+?([-0-9]+),\s+Y\+?([-0-9]+)\s+Button B:\s+X\+?([-0-9]+),\s+Y\+?([-0-9]+)\s+Prize:\s+X=([0-9]+),\s+Y=([0-9]+)\s*").unwrap()
});

impl FromStr for Puzzle {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_, [ax, ay, bx, by, px, py]) = PUZZLE_RE
            .captures(s)
            .ok_or(anyhow!("could not parse paragraph"))?
            .extract();
        Ok(Puzzle {
            ax: ax.parse()?,
            ay: ay.parse()?,
            bx: bx.parse()?,
            by: by.parse()?,
            px: px.parse()?,
            py: py.parse()?,
        })
    }
}

fn main() -> anyhow::Result<()> {
    let mut puzzle = Puzzle::parse_from_paragraph(io::stdin().lines())?;

    let puzzle_a: i64 = puzzle.iter().filter_map(|x| x.score()).sum();
    let puzzle_b: i64 = puzzle
        .iter_mut()
        .filter_map(|x| {
            x.for_puzzle_b();
            x.score()
        })
        .sum();

    Ok(println!("{}\n{}", puzzle_a, puzzle_b))
}
