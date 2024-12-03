pub mod year_2024 {
    use anyhow::anyhow;
    use std::io;

    pub mod day_01 {
        use super::*;

        pub fn parse() -> anyhow::Result<(Vec<u32>, Vec<u32>)> {
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

    pub mod day_02 {
        use super::*;

        pub fn parse() -> anyhow::Result<Vec<Vec<u8>>> {
            let mut result = vec![];
            let mut buffer = String::new();

            while io::stdin().read_line(&mut buffer)? > 0 {
                let s = buffer.split_ascii_whitespace().map(|x| x.parse()).collect::<Result<_,_>>()?;
                result.push(s);
                buffer.clear();
            }
            Ok(result)
        }
    }
}
