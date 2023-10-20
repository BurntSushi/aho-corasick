use std::io::Write;

use shared::{Benchmark, Sample};

fn main() -> anyhow::Result<()> {
    let Some(arg) = std::env::args_os().nth(1) else {
        anyhow::bail!("Usage: runner (<engine-name> | --version)")
    };
    let Ok(arg) = arg.into_string() else {
        anyhow::bail!("argument given is not valid UTF-8")
    };
    if arg == "--version" {
        writeln!(std::io::stdout(), env!("CARGO_PKG_VERSION"))?;
        return Ok(());
    }
    let engine = arg;
    let b = Benchmark::from_stdin()?;
    let samples = match (&*engine, &*b.model) {
        ("ascii-chars-prebuilt", "count") => memmem_prebuilt_count(&b)?,
        ("ascii-chars-oneshot", "count") => memmem_oneshot_count(&b)?,
        (engine, model) => {
            anyhow::bail!("unrecognized engine '{engine}' and model '{model}'")
        }
    };
    let mut stdout = std::io::stdout().lock();
    for s in samples.iter() {
        writeln!(stdout, "{},{}", s.duration.as_nanos(), s.count)?;
    }
    Ok(())
}

fn memmem_prebuilt_count(b: &Benchmark) -> anyhow::Result<Vec<Sample>> {
    let Ok(haystack) = std::str::from_utf8(&b.haystack) else {
        anyhow::bail!("jetscii ASCII search requires valid UTF-8 haystack")
    };
    let (needles, len) = needle_array(b)?;
    let fallback = jetscii_fallback(b)?;
    let finder = jetscii::AsciiChars::new(needles, len, fallback);
    shared::run(b, || {
        let mut haystack = haystack;
        let mut count = 0;
        while let Some(i) = finder.find(haystack) {
            count += 1;
            haystack = &haystack[i + 1..];
        }
        Ok(count)
    })
}

fn memmem_oneshot_count(b: &Benchmark) -> anyhow::Result<Vec<Sample>> {
    let Ok(haystack) = std::str::from_utf8(&b.haystack) else {
        anyhow::bail!("jetscii ASCII search requires valid UTF-8 haystack")
    };
    let (needles, len) = needle_array(b)?;
    let fallback = jetscii_fallback(b)?;
    shared::run(b, || {
        let finder = jetscii::AsciiChars::new(needles, len, &fallback);
        let mut haystack = haystack;
        let mut count = 0;
        while let Some(i) = finder.find(haystack) {
            count += 1;
            haystack = &haystack[i + 1..];
        }
        Ok(count)
    })
}

/// Converts the needles from the given benchmark into a fixed size 16-element
/// array along with the number of actual needles in the array (which may be
/// less than 16).
///
/// If any needle is more than one byte or there are too many needles to fit
/// into a 16-element array, then this returns an error. This also returns an
/// error if any of the bytes are not ASCII.
fn needle_array(b: &Benchmark) -> anyhow::Result<([u8; 16], i32)> {
    let mut array = [0u8; 16];
    let needles = b.needle_bytes()?;
    let Ok(len) = i32::try_from(needles.len()) else {
        anyhow::bail!("needle length {} could not fit into i32", needles.len())
    };
    anyhow::ensure!(
        needles.len() <= 16,
        "jetscii only supports at most 16 single byte needles, \
         but found {} needles",
        needles.len(),
    );
    for (i, byte) in needles.into_iter().enumerate() {
        array[i] = byte;
    }
    Ok((array, len))
}

/// Create a fallback predicate for jetscii's up-to-16-bytes search.
fn jetscii_fallback(b: &Benchmark) -> anyhow::Result<impl Fn(u8) -> bool> {
    let mut set = vec![false; 256];
    for byte in b.needle_bytes()? {
        set[usize::from(byte)] = true;
    }
    Ok(move |byte| set[usize::from(byte)])
}
