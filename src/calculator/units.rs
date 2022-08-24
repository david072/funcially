const UNITS: [&str; 2] = ["m", "mi"];

pub fn is_valid_unit(s: &str) -> bool {
    UNITS.contains(&s)
}

pub fn convert(src_unit: &str, dst_unit: &str, n: f64) -> f64 {
    match (src_unit, dst_unit) {
        ("m", "mi") => n / 5.0,
        ("mi", "m") => n * 5.0,
        _ => 0.0
    }
}