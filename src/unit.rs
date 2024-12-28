#[derive(Debug, Clone)]
pub struct Unit {
    pub filename: String,
    pub source: String,
}

impl Unit {
    pub fn new(filename: &str, source: &str) -> Unit {
        Unit {
            filename: filename.to_string(),
            source: source.to_string(),
        }
    }
}
