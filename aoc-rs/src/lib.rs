pub mod year_2024 {
    use anyhow::anyhow;
    use std::io;

    pub fn parse01() -> anyhow::Result<(Vec<u32>, Vec<u32>)> {
        let mut buffer = String::new();
        let mut left = vec![];
        let mut right = vec![];
        while io::stdin().read_line(&mut buffer)? > 0 {
            let mut s = buffer.split_ascii_whitespace();
            left.push(
                s.next()
                    .ok_or(anyhow!("left column not found"))?
                    .parse()?,
            );
            right.push(
                s.next()
                    .ok_or(anyhow!("right column not found"))?
                    .parse()?,
            );
            buffer.clear();
        }
        Ok((left, right))
    }
}