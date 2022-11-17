/// Double is an f64 that implements Eq and Hash, because Lua allows float indicies in tables.
/// See https://stackoverflow.com/questions/39638363/how-can-i-use-a-hashmap-with-f64-as-key-in-rust for more information.
#[derive(Debug, Copy, Clone)]
pub struct Double(pub f64);

impl Double {
    fn key(&self) -> u64 {
        self.0.to_bits()
    }
}

impl std::hash::Hash for Double {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.key().hash(state)
    }
}

impl PartialEq for Double {
    fn eq(&self, other: &Double) -> bool {
        self.key() == other.key()
    }
}

impl Eq for Double {}
