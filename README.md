# quasimodo

[![Build Status](https://travis-ci.org/keynmol/quasimodo.svg?branch=master)](https://travis-ci.org/keynmol/quasimodo)


```bash
# property tests
sbt 'main/runMain quasimodo.example.ExamplePropertySpec'
# [info] running quasimodo.example.ExamplePropertySpec
#  + OK, passed 100 tests.
#  + OK, passed 100 tests.

# regular tests
sbt 'main/runMain quasimodo.example.ExampleSpec'
# [info] running quasimodo.example.ExampleSpec
# All tests succeded
```
