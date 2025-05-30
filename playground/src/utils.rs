pub(crate) fn format_duration(duration: core::time::Duration) -> String {
    let secs = duration.as_secs();
    let nanos = duration.subsec_nanos();

    let fractional = nanos as f64 / 1_000_000_000.0; // Convert to seconds
    let formatted_fractional = format!("{:.3}", fractional); // Format to 3 decimal places
    let formatted_fractional = formatted_fractional.trim_start_matches("0.");

    format!("{}.{}", secs, formatted_fractional)
}
