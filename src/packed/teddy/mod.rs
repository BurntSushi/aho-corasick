mod generic;

/*
#[derive(Debug, Clone)]
pub(crate) struct Searcher {
    obj: Arc<dyn SearcherT>,
    minimum_len: usize,
    memory_usage: usize,
}

impl Searcher {
    pub(crate) fn find_at(
        &self,
        pats: &Patterns,
        haystack: &[u8],
        at: usize,
    ) -> Option<Match> {
        self.obj.find_at(pats, haystack, at)
    }

    pub(crate) fn minimum_len(&self) -> usize {
        self.minimum_len
    }

    pub(crate) fn memory_usage(&self) -> usize {
        self.memory_usage
    }
}

trait SearcherT: Debug + Send + Sync + UnwindSafe + RefUnwindSafe {
    fn find_at(
        &self,
        pats: &Patterns,
        haystack: &[u8],
        at: usize,
    ) -> Option<Match>;
}
*/
