# consonance
Consonance testing in R

![Status](https://travis-ci.org/tkonopka/consonance.svg?branch=master)
[![codecov](https://codecov.io/gh/tkonopka/consonance/branch/master/graph/badge.svg)](https://codecov.io/gh/tkonopka/consonance)

'Consonance' means [accord or agreement](https://www.dictionary.com/browse/consonance).
This R package associates the term 'consonance testing' to the process of 
testing that a dataset is compatible with predefined criteria. This type of 
testing can be used, for example, to determine if a trained model can produce
reliable results on a new dataset.  


## Installation & Usage

The package can be installed through github. 

```
library(devtools)
install_github("tkonopka/consonance")
library(consonance)
```

A minimal usage example to test that a data frame consists contains an id 
column and a numeric data column is as follows.

```
# a small data frame for this example
df <- data.frame(id=c("a", "b"), value=c(1, 2), stringsAsFactors=FALSE)
# define a suite of tests
suite <- consonance_test("id column character", is.character, .var="id") +
         consonance_test("value is numeric", is.numeric, .var="value")
# preview a summary of the suite
suite
# evaluate a data frame
test_consonance(df, suite)
```

The package vignette explains all the package features, including how
to incorporate `checkmate` assertions into a suite of consonance tests, how to
set importance levels for individual test conditions, and how to attach a
consonance suite to models (e.g. `lm`, `glm`, etc.)

The primary package vignette and a vignette dedicated to performance 
mesaurements explain how this package relates to other packages dealing with 
unit testing, argument checking and assertions.

