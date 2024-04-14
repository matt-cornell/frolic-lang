/// With rayon disabled, just call our operator function on the whole input
#[cfg(not(feature = "rayon"))]
pub fn dispatch_chunks<'a, T, R>(
    input: &'a [T],
    _next: impl Fn(usize, &'a [T]) -> usize,
    op: impl Fn(&'a [T], usize) -> Vec<R>,
    _min_block: usize,
) -> Vec<R> {
    op(input, 0)
}

/// With rayon enabled, use `next` to determine where the next index should be.
#[cfg(feature = "rayon")]
pub fn dispatch_chunks<'a, T: Sync + 'a, R: Send>(
    input: &'a [T],
    next: impl Fn(usize, &'a [T]) -> usize,
    op: impl Fn(&'a [T], usize) -> Vec<R> + Send + Sync,
    min_block: usize,
) -> Vec<R> {
    use rayon::prelude::*;
    let mut par_count = rayon::current_num_threads();
    let mut chunks = Vec::with_capacity((input.len() + par_count - 1) / par_count);
    let mut i = 0;
    while i < input.len() {
        let start = i;
        i += std::cmp::max((input.len() - i) / par_count, min_block);
        if i >= input.len() {
            chunks.push((&input[start..], start));
            break;
        }
        par_count -= 1;
        i = next(i, input);
        chunks.push((&input[start..i], start));
    }
    match &chunks[..] {
        [] => vec![],
        [ch] => op(ch.0, 0),
        _ => {
            let mut out = Vec::with_capacity(chunks.len());
            chunks
                .into_par_iter()
                .map(|(src, off)| op(src, off))
                .collect_into_vec(&mut out);
            out.into_iter()
                .reduce(|mut a, mut b| {
                    a.append(&mut b);
                    a
                })
                .unwrap()
        }
    }
}
