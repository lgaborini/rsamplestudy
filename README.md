# rsamplestudy

This package contains utilities to simulate data from two-level hierarchical models.

It is used in a evaluative forensic setting, where two sets of samples are extracted and compared.

It will be coupled to other packages where the said models are evaluated in a Bayesian way.

[![Travis build status](https://travis-ci.org/lgaborini/rsamplestudy.svg?branch=master)](https://travis-ci.org/lgaborini/rsamplestudy)
[![Codecov test coverage](https://codecov.io/gh/lgaborini/rsamplestudy/branch/master/graph/badge.svg)](https://codecov.io/gh/lgaborini/rsamplestudy?branch=master)

## Installation

The package is not yet on CRAN as it is very experimental.   
You can install the development version from this repository using `devtools` or `remotes`:

```
# install.packages('remotes')
remotes::install_github('lgaborini/rsamplestudy')
```

## Documentation

Available documentation can be found [here](https://lgaborini.github.io/rsamplestudy/).

## Contents

The package contains:

- utilities to sample from known distributions
- utilities to generate a full set of data according to a two-level model
- utilities to partition the full set of data into a reference/questioned/background partition

## Implemented models

- Dirichlet-Dirichlet model
- Normal-Normal model

