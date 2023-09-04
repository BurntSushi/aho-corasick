use std::io::Write;

use {
    aho_corasick::{
        AhoCorasick, AhoCorasickBuilder, AhoCorasickKind, MatchKind,
    },
    anyhow::Context,
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
        // These first 6 configurations are meant to test the default settings
        // on each of {compile, count} x {standard, leftmost-{first,longest}}.
        // We don't also test each of them with {nfa/(non-)?contiguous, dfa}
        // because it would just get ridiculous.
        ("compile", "default/standard") => {
            model_compile_ac(&b, || Ok(builder_ac(&b)?.build(&b.needles)?))?
        }
        ("compile", "default/leftmost-first") => model_compile_ac(&b, || {
            Ok(builder_ac(&b)?
                .match_kind(MatchKind::LeftmostFirst)
                .build(&b.needles)?)
        })?,
        ("compile", "default/leftmost-longest") => {
            model_compile_ac(&b, || {
                Ok(builder_ac(&b)?
                    .match_kind(MatchKind::LeftmostLongest)
                    .build(&b.needles)?)
            })?
        }
        ("count", "default/standard") => {
            let ac = builder_ac(&b)?.build(&b.needles)?;
            model_count_ac(&b, &ac)?
        }
        ("count", "default/leftmost-first") => {
            let ac = builder_ac(&b)?
                .match_kind(MatchKind::LeftmostFirst)
                .build(&b.needles)?;
            model_count_ac(&b, &ac)?
        }
        ("count", "default/leftmost-longest") => {
            let ac = builder_ac(&b)?
                .match_kind(MatchKind::LeftmostLongest)
                .build(&b.needles)?;
            model_count_ac(&b, &ac)?
        }

        // OK, now we start testing the specific Aho-Corasick automatons, but
        // we just focus on leftmost-first because that's the case we tend to
        // be more interested in optimizing in practice. There's also likely
        // to not be much of a perf difference between leftmost-first and
        // leftmost-longest.
        //
        // We also specifically disable prefilters so that we know we're always
        // measuring the actual automaton. (The 'default' engines above might
        // use a prefilter!)
        ("count", "nfa-noncontiguous/leftmost-first") => {
            let ac = builder_ac(&b)?
                .prefilter(false)
                .kind(Some(AhoCorasickKind::NoncontiguousNFA))
                .match_kind(MatchKind::LeftmostFirst)
                .build(&b.needles)?;
            model_count_ac(&b, &ac)?
        }
        ("count", "nfa-contiguous/leftmost-first") => {
            let ac = builder_ac(&b)?
                .prefilter(false)
                .kind(Some(AhoCorasickKind::ContiguousNFA))
                .match_kind(MatchKind::LeftmostFirst)
                .build(&b.needles)?;
            model_count_ac(&b, &ac)?
        }
        ("count", "dfa/leftmost-first") => {
            let ac = builder_ac(&b)?
                .prefilter(false)
                .kind(Some(AhoCorasickKind::DFA))
                .match_kind(MatchKind::LeftmostFirst)
                .build(&b.needles)?;
            model_count_ac(&b, &ac)?
        }

        // And now the packed substring routines. We include a 'compile'
        // model here as well because it's nice to know how long, specifically,
        // the packed searcher take to build in isolation.
        ("compile", "packed/leftmost-first") => {
            model_compile_packed(&b, || {
                let searcher = aho_corasick::packed::Config::new()
                    .match_kind(aho_corasick::packed::MatchKind::LeftmostFirst)
                    .builder()
                    .extend(&b.needles)
                    .build()
                    .ok_or_else(|| {
                        anyhow::anyhow!("could not build packed searcher")
                    })?;
                Ok(searcher)
            })?
        }
        ("count", "packed/leftmost-first") => {
            let searcher = aho_corasick::packed::Config::new()
                .match_kind(aho_corasick::packed::MatchKind::LeftmostFirst)
                .builder()
                .extend(&b.needles)
                .build()
                .ok_or_else(|| {
                    anyhow::anyhow!("could not build packed searcher")
                })?;
            model_count_packed(&b, &searcher)?
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

/// Implements the "compile a matcher" model for `AhoCorasick`.
fn model_compile_ac(
    b: &Benchmark,
    compile: impl FnMut() -> anyhow::Result<AhoCorasick>,
) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    shared::run_and_count(
        b,
        |re: AhoCorasick| Ok(re.find_iter(haystack).count()),
        compile,
    )
}

/// Implements the "compile a matcher" model for packed substring search.
fn model_compile_packed(
    b: &Benchmark,
    compile: impl FnMut() -> anyhow::Result<aho_corasick::packed::Searcher>,
) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    shared::run_and_count(
        b,
        |re: aho_corasick::packed::Searcher| {
            Ok(re.find_iter(haystack).count())
        },
        compile,
    )
}

/// Implements the "count all matches" model for `AhoCorasick`.
fn model_count_ac(
    b: &Benchmark,
    ac: &AhoCorasick,
) -> anyhow::Result<Vec<Sample>> {
    let haystack = &*b.haystack;
    shared::run(b, || Ok(ac.find_iter(haystack).count()))
}

/// Implements the "count all matches" model for packed substring search.
fn model_count_packed(
    b: &Benchmark,
    searcher: &aho_corasick::packed::Searcher,
) -> anyhow::Result<Vec<Sample>> {
    anyhow::ensure!(
        !b.case_insensitive,
        "rust/aho-corasick/packed engines are incompatible \
         with 'case-insensitive = true'"
    );

    let haystack = &*b.haystack;
    shared::run(b, || Ok(searcher.find_iter(haystack).count()))
}

/// Returns a default builder with as many settings as possible applied from
/// the benchmark definition. If the settings from the definition are not
/// supported, then this returns an error.
fn builder_ac(b: &Benchmark) -> anyhow::Result<AhoCorasickBuilder> {
    anyhow::ensure!(
        !(b.unicode && b.case_insensitive),
        "rust/aho-corasick engines are incompatible with 'unicode = true' and \
         'case-insensitive = true'"
    );
    let mut builder = AhoCorasick::builder();
    builder.ascii_case_insensitive(b.case_insensitive);
    Ok(builder)
}
