use std::io::Write;

use {
    anyhow::Context,
    daachorse::{
        bytewise::{DoubleArrayAhoCorasick, DoubleArrayAhoCorasickBuilder},
        MatchKind,
    },
    lexopt::{Arg, ValueExt},
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
        ("compile", "bytewise/standard") => {
            model_compile_bytewise_standard(&b)?
        }
        ("compile", "bytewise/leftmost-first") => {
            model_compile_bytewise_leftmost(&b, MatchKind::LeftmostFirst)?
        }
        ("compile", "bytewise/leftmost-longest") => {
            model_compile_bytewise_leftmost(&b, MatchKind::LeftmostLongest)?
        }
        ("count", "bytewise/standard") => model_count_bytewise_standard(&b)?,
        ("count", "bytewise/overlapping") => {
            model_count_bytewise_overlapping(&b)?
        }
        ("count", "bytewise/leftmost-first") => {
            model_count_bytewise_leftmost(&b, MatchKind::LeftmostFirst)?
        }
        ("count", "bytewise/leftmost-longest") => {
            model_count_bytewise_leftmost(&b, MatchKind::LeftmostLongest)?
        }
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

/// Implements the "compile a matcher" model for a bytewise daachorse automaton
/// using "standard" (i.e., what's found in a textbook description of
/// Aho-Corasick for a non-overlapping search) match semantics.
fn model_compile_bytewise_standard(
    b: &Benchmark,
) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    shared::run_and_count(
        b,
        |ac: daachorse::DoubleArrayAhoCorasick<u32>| {
            Ok(ac.find_iter(haystack).count())
        },
        || compile_bytewise(b, MatchKind::Standard),
    )
}

/// Implements the "compile a matcher" model for a bytewise daachorse automaton
/// using the given match semantics. The match semantics must be either
/// leftmost-first or leftmost-longest.
fn model_compile_bytewise_leftmost(
    b: &Benchmark,
    kind: MatchKind,
) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    shared::run_and_count(
        b,
        |ac: daachorse::DoubleArrayAhoCorasick<u32>| {
            Ok(ac.leftmost_find_iter(haystack).count())
        },
        || compile_bytewise(b, kind),
    )
}

/// Implements a multi-substring algorithm using daachorse's bytewise
/// Aho-Corasick automaton. This uses "standard" match semantics.
fn model_count_bytewise_standard(
    b: &Benchmark,
) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    let ac = compile_bytewise(b, MatchKind::Standard)?;
    shared::run(b, || Ok(ac.find_iter(haystack).count()))
}

/// Implements a multi-substring algorithm using daachorse's bytewise
/// Aho-Corasick automaton. This uses "standard" match semantics and finds all
/// overlapping matches.
fn model_count_bytewise_overlapping(
    b: &Benchmark,
) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    let ac = compile_bytewise(b, MatchKind::Standard)?;
    shared::run(b, || Ok(ac.find_overlapping_iter(haystack).count()))
}

/// Implements a multi-substring algorithm using daachorse's bytewise
/// Aho-Corasick automaton. This requires leftmost-first or leftmost-longest
/// match semantics.
fn model_count_bytewise_leftmost(
    b: &Benchmark,
    kind: MatchKind,
) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    let ac = compile_bytewise(b, kind)?;
    shared::run(b, || Ok(ac.leftmost_find_iter(haystack).count()))
}

/// Compiles a naive multi-substring matcher by building a single substring
/// matcher for each needle.
fn compile_bytewise(
    b: &Benchmark,
    kind: MatchKind,
) -> anyhow::Result<DoubleArrayAhoCorasick<u32>> {
    anyhow::ensure!(
        !b.case_insensitive,
        "daachorse doesn't support case insensitive mode",
    );
    let result = DoubleArrayAhoCorasickBuilder::new()
        .match_kind(kind)
        .build(&b.needles);
    let ac = match result {
        Ok(ac) => ac,
        Err(err) => anyhow::bail!("daachorse build failed: {}", err),
    };
    Ok(ac)
}
