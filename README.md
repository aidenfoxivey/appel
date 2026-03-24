Code in `/sample_programs` is copyright of Andrew W. Appel. Originally sourced from [his site](https://www.cs.princeton.edu/~appel/modern/testcases/).

To format, run `dune build @fmt` and then `dune promote`.

To execute main, run `dune exec appel`.

To execute tests, run `dune test`.

In `.git/hooks/pre-commit`, I have put:

```
#!/bin/sh
dune build @fmt
dune test
```