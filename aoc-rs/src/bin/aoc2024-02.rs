use std::io;

pub fn parse() -> anyhow::Result<Vec<Vec<u8>>> {
    let mut result = vec![];
    let mut buffer = String::new();

    while io::stdin().read_line(&mut buffer)? > 0 {
        let s = buffer
            .split_ascii_whitespace()
            .map(|x| x.parse())
            .collect::<Result<_, _>>()?;
        result.push(s);
        buffer.clear();
    }
    Ok(result)
}

fn is_safe2(input: &[u8]) -> bool {
    input
        .iter()
        .zip(input.iter().skip(1))
        .map(|(l, r)| *r as i16 - *l as i16)
        .try_fold(0, |acc, x| {
            if (acc != -x.signum()) && (1..=3).contains(&x.abs()) {
                Some(x.signum())
            } else {
                None
            }
        })
        .is_some()
}

fn main() -> anyhow::Result<()> {
    let input = parse()?;
    let puzzle_a = input.clone().into_iter().filter(|x| is_safe2(x)).count();
    let puzzle_b = input
        .into_iter()
        .map(|x| {
            // create versions with one element removed
            (0..x.len()).map(move |i| {
                let mut new_x = x.clone();
                new_x.remove(i);
                new_x
            })
        })
        .filter_map(|mut xs| xs.any(|x| is_safe2(&x)).then(|| ()))
        .count();

    Ok(println!("{}\n{}", puzzle_a, puzzle_b))
}
