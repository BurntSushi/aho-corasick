use std::io::Write;

use {
    anyhow::Context,
    lexopt::{Arg, ValueExt},
    memchr::memmem,
};

use shared::{Benchmark, Sample};

fn main() -> anyhow::Result<()> {
    let mut p = lexopt::Parser::from_env();
    let (mut engine, mut quiet) = (String::new(), false);
    while let Some(arg) = p.next()? {
        match arg {
            Arg::Short('h') | Arg::Long("help") => {
                anyhow::bail!("main [--version | --quiet] <engine>")
            }
            Arg::Short('q') | Arg::Long("quiet") => {
                quiet = true;
            }
            Arg::Long("version") => {
                writeln!(std::io::stdout(), "{}", env!("CARGO_PKG_VERSION"))?;
                return Ok(());
            }
            Arg::Value(v) => {
                anyhow::ensure!(
                    engine.is_empty(),
                    "only one engine string allowed"
                );
                engine = v.string().context("<engine>")?;
                anyhow::ensure!(
                    !engine.is_empty(),
                    "engine string cannot be empty"
                );
            }
            _ => return Err(arg.unexpected().into()),
        }
    }

    let b = Benchmark::from_stdin()
        .context("failed to read KLV data from <stdin>")?;
    let samples = match (b.model.as_str(), engine.as_str()) {
        ("compile", "rust/memchr/memmem") => model_compile_memmem(&b)?,
        ("count", "rust/memchr/memmem") => model_count_memmem(&b)?,
        ("count", "rust/std") => model_count_std(&b)?,
        _ => anyhow::bail!(
            "unsupported model/engine pair, model={} engine={}",
            b.model,
            engine
        ),
    };
    if !quiet {
        let mut stdout = std::io::stdout().lock();
        for s in samples.iter() {
            writeln!(stdout, "{},{}", s.duration.as_nanos(), s.count)?;
        }
    }
    Ok(())
}

/// Implements the "compile a matcher" model for naive multi-substring search
/// with the `memchr` crate's `memmem` implementation.
fn model_compile_memmem(b: &Benchmark) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    shared::run_and_count(
        b,
        |finders: Vec<memmem::Finder<'static>>| {
            let mut count = 0;
            for f in finders.iter() {
                count += f.find_iter(haystack).count();
            }
            Ok(count)
        },
        || compile_memmem(b),
    )
}

/// Implements a naive multi-substring algorithm using the `memchr` crate's
/// `memmem` implementation.
fn model_count_memmem(b: &Benchmark) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    let finders = compile_memmem(b)?;
    shared::run(b, || {
        let mut count = 0;
        for f in finders.iter() {
            count += f.find_iter(haystack).count();
        }
        Ok(count)
    })
}

/// Implements a naive multi-substring algorithm using std's single substring
/// search implementation. This returns an error if the haystack or any of
/// the needles are invalid UTF-8.
fn model_count_std(b: &Benchmark) -> anyhow::Result<Vec<Sample>> {
    let Ok(haystack) = std::str::from_utf8(&b.haystack) else {
        anyhow::bail!("haystack is not valid UTF-8")
    };
    let mut needles = vec![];
    for needle in b.needles.iter() {
        let Ok(needle) = std::str::from_utf8(needle) else {
            anyhow::bail!("one of the needles is not valid UTF-8")
        };
        needles.push(needle);
    }
    shared::run(b, || {
        let mut count = 0;
        for needle in needles.iter() {
            count += haystack.matches(needle).count();
        }
        Ok(count)
    })
}

/// Compiles a naive multi-substring matcher by building a single substring
/// matcher for each needle.
fn compile_memmem(
    b: &Benchmark,
) -> anyhow::Result<Vec<memmem::Finder<'static>>> {
    anyhow::ensure!(
        !b.case_insensitive,
        "naive multi-substring search doesn't support case insensitive mode",
    );
    let mut finders = vec![];
    for needle in b.needles.iter() {
        finders.push(memmem::Finder::new(needle).into_owned());
    }
    Ok(finders)
}
