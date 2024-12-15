use itertools::Itertools;
use std::str::FromStr;

pub trait ParseFrom {
    fn parse_from_lines<B, E>(source: B) -> anyhow::Result<Vec<Self>>
    where
        B: Iterator<Item = Result<String, E>>,
        Self: Sized,
        anyhow::Error: From<E>;

    fn parse_from_paragraph<B, E>(source: B) -> anyhow::Result<Vec<Self>>
    where
        B: Iterator<Item = Result<String, E>>,
        Self: Sized,
        anyhow::Error: From<E>;
}

impl<T> ParseFrom for T
where
    T: FromStr,
    anyhow::Error: From<<T as FromStr>::Err>,
{
    fn parse_from_lines<B, E>(source: B) -> anyhow::Result<Vec<Self>>
    where
        B: Iterator<Item = Result<String, E>>,
        Self: Sized,
        anyhow::Error: From<E>,
    {
        source
            .map(|line| Ok(line?.parse()?))
            .collect::<Result<Vec<_>, _>>()
    }

    fn parse_from_paragraph<B, E>(source: B) -> anyhow::Result<Vec<Self>>
    where
        B: Iterator<Item = Result<String, E>>,
        Self: Sized,
        anyhow::Error: From<E>,
    {
        source
            .chunk_by(|line| line.as_ref().ok().map(String::is_empty))
            .into_iter()
            .filter_map(|q| match q.0 {
                Some(true) => None,
                Some(false) => Some(Ok(q.1)),
                None => Some(Err(q.1)),
            })
            .map(|r| {
                Ok(r.unwrap_or_else(|e| e)
                    .collect::<Result<Vec<_>, _>>()?
                    .join("\n")
                    .parse()?)
            })
            .collect()
    }
}
