<!-- README.md is generated from README.Rmd. Do not edit manually -->

# consonance

Consonance testing in
R

[![Status](https://travis-ci.org/tkonopka/consonance.svg?branch=master)](https://travis-ci.org/github/tkonopka/consonance)
[![codecov](https://codecov.io/gh/tkonopka/consonance/branch/master/graph/badge.svg)](https://codecov.io/gh/tkonopka/consonance)

‘Consonance’ means [accord or
agreement](https://www.dictionary.com/browse/consonance). This R package
associates the term ‘consonance testing’ to the process of determining
that a dataset is compatible with predefined criteria. This type of
testing can be used, for example, to determine if a new dataset should
proceed to statistical analysis or if a new dataset satisfies the
assumptions built into a trained model.

Consonance testing is related to unit testing and argument testing. The
distinction is that consonance testing is relevant during data analysis
rather than during software development. Accordingly, the `consonance`
package provides a syntax and features that are geared toward
interactive uses and toward pipelines.

## Installation

The package can be installed through github.

``` r
library(devtools)
install_github("tkonopka/consonance")
```

## Usage

For a minimal usage example, consider a small data frame with a
character-class column (id) and a numeric-class column
(value).

``` r
example_data <- data.frame(id=c("a", "b"), value=c(1, 2), stringsAsFactors=FALSE)
example_data
```

    ##   id value
    ## 1  a     1
    ## 2  b     2

The package can be used to create a suite of tests.

``` r
library(consonance)
suite <- consonance_test("id column character", is.character, .var="id") +
  consonance_test("value numeric", is.numeric, .var="value")
suite
```

    ## Consonance test suite with 2 tests

The data can then be evaluated against the suite.

``` r
# with default settings, validation of consonant data is quiet
validate(example_data, suite)
# logging can be activated to provide verbose messages
validate(example_data, suite, logging.level="INFO")
```

    ## INFO  [2020-10-08 13:34:28] validate(example_data, suite)
    ## INFO  [2020-10-08 13:34:28] consonance pass: id column character
    ## INFO  [2020-10-08 13:34:28] consonance pass: value numeric
    ## INFO  [2020-10-08 13:34:28] consonance object:   example_data
    ## INFO  [2020-10-08 13:34:28] consonance suite:    suite
    ## INFO  [2020-10-08 13:34:28] consonance result:   2 OK, 0 warnings, 0 errors

When the data object does not meet the consonance criteria, the
validation step provides log messages and halts execution.

``` r
validate(example_data[, "id", drop=FALSE], suite)
```

    ## ERROR [2020-10-08 13:34:28] consonance error:    value numeric
    ## ERROR [2020-10-08 13:34:28] consonance object:   example_data[, "id", drop = FALSE]
    ## ERROR [2020-10-08 13:34:28] consonance suite:    suite
    ## ERROR [2020-10-08 13:34:28] consonance result:   1 OK, 0 warnings, 1 error

    ## Error: consonance

The package vignettes explain more package features, including:

  - using consonance suites within pipelines (with `%>%`)
  - toggling testing on/off
  - setting importance levels for individual test conditions
  - using `checkmate` and custom functions in consonance tests
  - attaching consonance suites to models (e.g. `lm`, `glm`, etc.)
  - reporting results into log files

Supplementary vignettes describe further topics:

  - advanced use of consonance testing for model objects
  - consonance validations in batch

## Related work

Several other R packages handle testing, argument checking, and data
validation. Some are listed here with a brief description.

  - [checkmate](https://github.com/mllg/checkmate/) - collection of fast
    data-checking functions; can be used together with `consonance`.
  - [assertthat](https://cran.r-project.org/web/packages/assertthat/) -
    framework for data testing; provides friendly error messages
  - [assertr](https://github.com/ropensci/assertr) - framework for data
    testing; geared toward dplyr pipelines.
  - [validate](https://github.com/data-cleaning/validate) -
    comprehensive framework for data testing; provides detailed feedback
    on errors; overlaps with `consonance` with a different syntax

A comparison of these packages in terms of performance is provided in a
vignette, `performance.Rmd`.

## Contributing

Comments and contributions are welcome. Please raise an issue.
