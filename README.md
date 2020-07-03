# consonance
Consonance testing in R

![Status](https://travis-ci.org/tkonopka/consonance.svg?branch=master)
[![codecov](https://codecov.io/gh/tkonopka/consonance/branch/master/graph/badge.svg)](https://codecov.io/gh/tkonopka/consonance)

'Consonance' means [accord or agreement](https://www.dictionary.com/browse/consonance).
This R package associates the term 'consonance testing' to the process of 
determining that a dataset is compatible with predefined criteria. This type of 
testing can be used, for example, to determine if a new dataset should proceed
to statistical analysis or if a new dataset satisfies the assumptions built
into a trained model. 

Consonance testing is related to unit testing and argument testing. The 
distinction is that consonance testing is relevant during data analysis rather 
than during software development. Accordingly, the `consonance` package 
provides a syntax and features that are geared toward interactive uses and 
toward pipelines. 


## Installation

The package can be installed through github. 

```
library(devtools)
install_github("tkonopka/consonance")
library(consonance)
```

## Usage

A minimal usage example is as follows. This tests that a data frame contains 
a character-class (id) and a numeric-class (value) column.

```
# create a small data frame for this example
df <- data.frame(id=c("a", "b"), value=c(1, 2), stringsAsFactors=FALSE)
# define a suite of tests
suite <- consonance_test("id column character", is.character, .var="id") +
         consonance_test("value numeric", is.numeric, .var="value")
# preview a summary of the suite
suite
# evaluate consonance of the data frame - should execute quietly
test_consonance(df, suite)
# evaluate on another data frame - should generate messages and an error
test_consonance(df[, "id", drop=FALSE], suite) 
```

The package vignette explains more package features, including how
to incorporate `checkmate` assertions into a suite of consonance tests, how to
set importance levels for individual test conditions, and how to attach a
consonance suite to models (e.g. `lm`, `glm`)

Supplementary vignettes describe further topics. One document assesses 
performance compared to base R and other testing frameworks. Another vignette
provides a practical example of how to use the package during data modeling.


## Contributing

Comments and contributions are welcome. Please raise an issue.

