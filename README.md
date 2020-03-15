# hwc

This is the common `wc` command, implemented in Haskell.
Turns out that it's about 4-6 times faster than GNU coreutils' `wc`,
depending on the exact subset of statistics being computed.

`hwc` supports counting bytes, words and lines, as well as UTF-8 characters and maximum line length.
It does so in a composable and _efficient_ manner, so that only those statistics requested by user
are actually calculated.

It all started from [this](https://chrispenner.ca/posts/wc) blog post.
The author of that post manages to beat the C version by resorting to parallel input file handling
(and that's a good demonstration, no critique here — go figure how to parallelize the C version correctly!),
but I was curious if it's possible to beat the (single-threaded) C version by a single-threaded Haskell version.

Turns out the answer is yes, and with some very minor modifications to the original author's implementation.

For more details about the implementation,
please refer to [this](https://0xd34df00d.me/posts/2020/03/the-joys-and-perils.html) post.
