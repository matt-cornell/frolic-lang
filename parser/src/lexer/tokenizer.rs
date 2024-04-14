use super::*;

fn tokenize_impl<'src, F, S: SpanConstruct<F>, E: ErrorReporter<TokenizeError<S>>>(
    input: &'src str,
    offset: usize,
    file: F,
    errs: E,
) -> Vec<Token<'src, S>> {
    todo!()
}
#[cfg(feature = "rayon")]
pub fn tokenize<'src, F: Copy + Send + Sync, S: SpanConstruct<F> + Send, E: Sync>(input: &'src str, file: F, errs: E) -> Vec<Token<'src, S>>
where
    for<'a> &'a E: ErrorReporter<TokenizeError<S>>,
{
    const STARTS: &[u8] = b"\n\t !$%&(),.:;@~";
    dispatch_chunks(
        input.as_bytes(),
        |offset, input| {
            input[offset..]
                .iter()
                .position(|b| STARTS.contains(b))
                .map_or(input.len(), |i| i + offset)
        },
        |src, offset| tokenize_impl(unsafe { std::str::from_utf8_unchecked(src) }, offset, file, &errs),
        1024,
    )
}
#[cfg(not(feature = "rayon"))]
pub fn tokenize<'src, F, S: SpanConstruct<F>, E: ErrorReporter<TokenizeError<F>>>(input: &'src str, file: F, errs: E) -> Vec<Token<S>> {
    tokenize_impl(src, offset, file, errs)
}
