use std::*;

/// Double is an f64 that implements Eq and Hash, because Lua allows float indicies in tables.
/// See https://stackoverflow.com/questions/39638363/how-can-i-use-a-hashmap-with-f64-as-key-in-rust for more information.
#[derive(Debug, Copy, Clone)]
pub struct Double(pub f64);

impl Double {
    fn key(&self) -> u64 {
        self.0.to_bits()
    }
}

impl hash::Hash for Double {
    fn hash<H>(&self, state: &mut H)
    where
        H: hash::Hasher,
    {
        self.key().hash(state)
    }
}

impl ops::Mul<Double> for Double {
    type Output = Double;

    fn mul(self, rhs: Double) -> Self::Output { 
        Double(self.0 * rhs.0)
    }
}

impl ops::Div<Double> for Double {
    type Output = Double;

    fn div(self, rhs: Double) -> Self::Output { 
        Double(self.0 / rhs.0)
    }
}

impl ops::Sub<Double> for Double {
    type Output = Double;

    fn sub(self, rhs: Double) -> Self::Output { 
        Double(self.0 - rhs.0)
    }
}

impl ops::Add<Double> for Double {
    type Output = Double;

    fn add(self, rhs: Double) -> Self::Output { 
        Double(self.0 + rhs.0)
    }
}

impl ops::Mul<f64> for Double {
    type Output = Double;

    fn mul(self, rhs: f64) -> Self::Output { 
        Double(self.0 * rhs)
    }
}

impl ops::Div<f64> for Double {
    type Output = Double;

    fn div(self, rhs: f64) -> Self::Output { 
        Double(self.0 / rhs)
    }
}

impl ops::Sub<f64> for Double {
    type Output = Double;

    fn sub(self, rhs: f64) -> Self::Output { 
        Double(self.0 - rhs)
    }
}

impl ops::Add<f64> for Double {
    type Output = Double;

    fn add(self, rhs: f64) -> Self::Output { 
        Double(self.0 + rhs)
    }
}

impl PartialEq for Double {
    fn eq(&self, other: &Double) -> bool {
        self.key() == other.key()
    }
}

impl Eq for Double {}
