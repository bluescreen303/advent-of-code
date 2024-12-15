use anyhow::anyhow;
use aoc_rs::ParseFrom;
use regex::Regex;
use std::{
    collections::{HashMap, HashSet},
    io,
    str::FromStr,
    sync::LazyLock,
    time::Duration,
};

const WIDTH: i32 = 101;
const HEIGHT: i32 = 103;
const HOR: u8 = WIDTH as u8 / 2;
const VER: u8 = HEIGHT as u8 / 2;

#[derive(PartialEq, Eq, Hash)]
enum Quadrant {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

#[derive(Clone, Copy)]
struct Robot {
    px: u8,
    py: u8,
    vx: i8,
    vy: i8,
}

impl Robot {
    fn run(&mut self, time: Duration) {
        let s = time.as_secs() as i32;
        self.px = ((self.px as i32 + s * self.vx as i32).rem_euclid(WIDTH))
            .try_into()
            .unwrap();
        self.py = ((self.py as i32 + s * self.vy as i32).rem_euclid(HEIGHT))
            .try_into()
            .unwrap();
    }

    fn quadrant(&self) -> Option<Quadrant> {
        if self.px < HOR && self.py < VER {
            Some(Quadrant::TopLeft)
        } else if self.px > HOR && self.py < VER {
            Some(Quadrant::TopRight)
        } else if self.px < HOR && self.py > VER {
            Some(Quadrant::BottomLeft)
        } else if self.px > HOR && self.py > VER {
            Some(Quadrant::BottomRight)
        } else {
            None
        }
    }
}

static ROBOT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"p=([0-9]+),([0-9]+) v=([-0-9]+),([-0-9]+)").unwrap());

impl FromStr for Robot {
    type Err = anyhow::Error;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let (_, [px, py, vx, vy]) = ROBOT_RE
            .captures(line)
            .ok_or(anyhow!("could not parse line"))?
            .extract();
        Ok(Robot {
            px: px.parse()?,
            py: py.parse()?,
            vx: vx.parse()?,
            vy: vy.parse()?,
        })
    }
}

#[derive(Clone)]
struct PlayingField(Vec<Robot>);

impl PlayingField {
    fn run(&mut self, time: Duration) {
        self.0.iter_mut().for_each(|robot| robot.run(time))
    }

    fn has_overlap(&self) -> bool {
        let mut locations = HashSet::new();

        self.0
            .iter()
            .any(|robot| !locations.insert((robot.px, robot.py)))
    }

    fn robots_per_quadrant(&self) -> HashMap<Quadrant, u32> {
        let mut quadrants: HashMap<Quadrant, u32> = HashMap::new();
        for robot in &self.0 {
            if let Some(q) = robot.quadrant() {
                *quadrants.entry(q).or_default() += 1;
            }
        }
        quadrants
    }

    fn parse_from_lines<B, E>(source: B) -> anyhow::Result<Self>
    where
        B: Iterator<Item = Result<String, E>>,
        Self: Sized,
        anyhow::Error: From<E>,
    {
        Robot::parse_from_lines(source).map(Self)
    }
}

fn main() -> anyhow::Result<()> {
    let mut input = PlayingField::parse_from_lines(io::stdin().lines())?;

    let mut puzzle_a = input.clone();
    puzzle_a.run(Duration::from_secs(100));
    let puzzle_a: u32 = puzzle_a.robots_per_quadrant().values().product();

    let mut puzzle_b = 0;
    while input.has_overlap() {
        input.run(Duration::from_secs(1));
        puzzle_b += 1;
    }

    Ok(println!("{}\n{}", puzzle_a, puzzle_b))
}
