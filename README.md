This is an incomplete implementation of Austin Henley's Teeny Tiny language. It's written in Haskell, is about a hundred lines long and was written in a couple of hours. The code is pretty messy.

Compared to Austin's implementation, this one has got a proper AST! That's about the only positive thing though ;)

The lexer/parser is built using [`Text.ParserCombinators.ReadP`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-ParserCombinators-ReadP.html) because I was way too lazy to use anything other than what's available in `base`.

It also doesn't support GOTOs and comments because, again, I was too lazy.

To use it, pipe your Teeny Tiny code to the standard input and you'll get your C code on the standard output.

Something like this will work just fine:

```bash
runhaskell <program.teeny teeny.hs | gcc -x c - ; ./a.out
```
