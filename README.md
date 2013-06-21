## Shen.hs

Shen.hs is a K Lambda interpreter written in Haskell. It's main goal is to run Shen language.

## Current status

Shen test suite runs with following result:

    passed ... 94.0
    failed ...34.0
    pass rate ...73.4375%

And benchmarks fails with: `UserError "==> is not a legitimate function name.\r\n"`.

## Installation

Shen.hs comes with required K Lambda source, but interpreter doesn't automatically load Shen sources.

Just clone the repository and install with `cabal install`. An executable with name `Shen.hs` will be installed. This executable actually runs the K Lambda interpreter, it doesn't load Shen sources.

To load Shen sources, run `(load-shen "path-to-K Lambda-folder")`. ie. `(load-shen "/home/omer/Shen 12/K Lambda")` in interpreter.

## Contributions

This project has been a long time hobby of mine, but I can't spare enough time to fix bugs and improve performance. There are lots of things to improve and fix in the source. Contributions are welcome, please visit issues page to see list of reported issues.
