use clap::Parser;

#[derive(Debug, Parser, Clone)]
pub struct CommonArgs {
    #[clap(long = "debug")]
    pub debug: bool,
    #[clap(
        long = "timings",
        default_missing_value = "5",
        require_equals = true,
        num_args = 0..=1
    )]
    pub timings: Option<usize>,
    #[clap(long = "threads", short = 'j', default_value = "0")]
    threads: usize,
}

impl CommonArgs {
    /// Sets up the parallelism and returns what you should pass to driver.
    /// You can call this function at most once.
    pub fn parallel(&self) -> bool {
        let mut builder = rayon::ThreadPoolBuilder::new();
        if self.threads != 0 {
            builder = builder.num_threads(self.threads);
        }
        // This fails if we call it twice, but we probably called it previously with the same
        // value, so don't worry about it.
        let _ = builder.build_global();
        self.threads != 1
    }
}
