# hs-monitoring

This project provides logging and benchmarking for the purpose of system and process monitoring.


## CI

|   | Linux | macOS | Windows |
|---|---|---|---|
| GHC 9.4.5 | [![Haskell CI (Linux)](https://github.com/CodiePP/hsMonitoring/actions/workflows/ci_linux.yml/badge.svg)](https://github.com/CodiePP/hsMonitoring/actions/workflows/ci_linux.yml) | [![Haskell CI (macOS)](https://github.com/CodiePP/hsMonitoring/actions/workflows/ci_macos.yml/badge.svg)](https://github.com/CodiePP/hsMonitoring/actions/workflows/ci_macos.yml)  |  [![Haskell CI (Windows)](https://github.com/CodiePP/hsMonitoring/actions/workflows/ci_windows.yml/badge.svg)](https://github.com/CodiePP/hsMonitoring/actions/workflows/ci_windows.yml) |

## build and test

`cabal build all`

`cabal test all`

or using _stack_:

`stack build`

`stack test`


## Disclaimer

This work is based on a fork from https://github.com/input-output-hk/iohk-monitoring-framework which is Copyright 2019 by IOHK.

This project might take a different approach and has other goals to meet than IOHK's. The interfaces might not be compatible with the requirements of their blockchain node.
