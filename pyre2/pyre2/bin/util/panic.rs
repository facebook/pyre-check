use std::backtrace::Backtrace;

pub fn exit_on_panic() {
    std::panic::set_hook(Box::new(move |info| {
        eprintln!("Thread panicked, shutting down: {}", info);
        // The Meta VS Code can't find files that look like
        // `./foo/bar.rs` in backtraces, so we replace them with `foo/bar.rs`.
        // Ideally we'd fix VS Code to understand `./` in paths, or fix
        // backtraces to not include the `./`.
        // But this improves our efficiency in the short term.
        eprintln!(
            "Backtrace:\n{}",
            Backtrace::force_capture()
                .to_string()
                .replace("at ./", "at ")
        );
        std::process::exit(1);
    }));
}
