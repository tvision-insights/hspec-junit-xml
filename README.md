# hspec-junit-xml

[![Build Status](https://travis-ci.org/tvision-insights/hspec-junit-xml.svg?branch=master)](https://travis-ci.org/tvision-insights/hspec-junit-xml)

Formatter for Hspec that creates test reports in the "JUnit" format that's understood by CI tools,
including the elasped time of each test. The elapsed time is also added to Hspec's `specdoc`
formatter's output for human consumption.

The time formatter can also be used separately.

## Example

A complete example of running a Spec can be found in [examples/](examples/test/Spec.hs). The output
is [examples/test-results/examples.xml](examples/test-results/examples.xml).
