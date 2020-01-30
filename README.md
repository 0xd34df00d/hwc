# hwc

This is the common `wc` command, implemented in Haskell.
Turns out that it's about 4-6 times faster than GNU coreutils' `wc`,
expressed in about 18 lines of more or less idiomatic Haskell.

For now, being rather a PoC,
`hwc` only supports counting bytes, words and lines, and does not have any command-line options.
Counting different types of statistics in a composable and _efficient_ manner is the actual next step for `hwc`.

This "project" is largely inspired by [this](https://chrispenner.ca/posts/wc) blog post.
The author of that post manages to beat the C version by resorting to parallel input file handling
(and that's a good demonstration, no critique here — go figure how to parallelize the C version correctly!),
but I was curious if it's possible to beat the (single-threaded) C version by a single-threaded Haskell version.

Turns out the answer is yes, and with some very minor modifications to the original author's implementation.

The final code turns out to be a bit less than idiomatic
(`Int`s instead of `Bool`s, some ugly `Int`-based math to avoid repacking into `Bool`s, stuff like that),
but:

* Even what I would call a totally idiomatic implementation still manages to achieve a significant (2x-3x) improvement over C/coreutils.
* We're competing with C after all.
