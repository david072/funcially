use ::common::{Result, ErrorType};

const UNITS: [&str; 14] = [
    "m", "mi", "ft", "in", "yd", // distance
    "s", "min", "h", // time
    "g", "lb", // mass
    "pa", "bar", // pressure
    "cal", "b", // misc
];

pub fn is_valid_unit(s: &str) -> bool {
    UNITS.contains(&s)
}

pub fn convert(src_unit: &str, dst_unit: &str, n: f64, range: &std::ops::Range<usize>) -> Result<f64> {
    match (src_unit, dst_unit) {
        // distance
        ("m", "mi") => Ok(n / 1609.344),
        ("m", "ft") => Ok(n * 3.281),
        ("m", "in") => Ok(n / 0.0254),
        ("m", "yd") => Ok(n / 0.9144),

        ("ft", "m") => Ok(n / 3.281),
        ("ft", "mi") => Ok(n / 5280.0),
        ("ft", "in") => Ok(n * 12.0),
        ("ft", "yd") => Ok(n / 3.0),

        ("mi", "m") => Ok(n * 1609.344),
        ("mi", "ft") => Ok(n * 5280.0),
        ("mi", "in") => Ok(n * 63360.0),
        ("mi", "yd") => Ok(n * 1760.0),

        ("in", "m") => Ok(n * 0.0254),
        ("in", "ft") => Ok(n / 12.0),
        ("in", "mi") => Ok(n / 63360.0),
        ("in", "yd") => Ok(n / 36.0),

        ("yd", "m") => Ok(n * 0.9144),
        ("yd", "ft") => Ok(n * 3.0),
        ("yd", "mi") => Ok(n / 1760.0),
        ("yd", "in") => Ok(n * 36.0),

        // time
        ("s", "min") => Ok(n / 60.0),
        ("s", "h") => Ok(n / 3600.0),

        ("min", "s") => Ok(n * 60.0),
        ("min", "h") => Ok(n / 60.0),

        ("h", "s") => Ok(n * 3600.0),
        ("h", "min") => Ok(n / 60.0),

        // mass
        ("g", "lb") => Ok(n / 453.59237),
        ("lb", "g") => Ok(n * 453.59237),

        // pressure
        ("pa", "bar") => Ok(n / 100_000.0),
        ("bar", "pa") => Ok(n * 100_000.0),
        _ => Err(ErrorType::UnknownConversion.with(range.clone())),
    }
}