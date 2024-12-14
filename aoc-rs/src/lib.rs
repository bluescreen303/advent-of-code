use std::error::Error;

pub trait ParseFrom {
    fn parse_from_lines<B, E>(source: B) -> anyhow::Result<Self>
    where
        B: Iterator<Item = Result<String, E>>,
        E: Error + Send + Sync + 'static,
        Self: Sized;
}
