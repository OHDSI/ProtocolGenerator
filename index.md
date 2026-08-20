# ProtocolGenerator

[![R-CMD-check](https://github.com/OHDSI/ProtocolGenerator/actions/workflows/R_CMD_check_Hades.yaml/badge.svg)](https://github.com/OHDSI/ProtocolGenerator/actions/workflows/R_CMD_check_Hades.yaml)
[![pkgdown](https://github.com/OHDSI/ProtocolGenerator/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/OHDSI/ProtocolGenerator/actions/workflows/pkgdown.yaml)
[![codecov](https://codecov.io/github/OHDSI/ProtocolGenerator/branch/main/graph/badge.svg)](https://app.codecov.io/github/OHDSI/ProtocolGenerator?branch=main)
[![GitHub R package
version](https://img.shields.io/github/r-package/v/OHDSI/ProtocolGenerator)](https://github.com/OHDSI/ProtocolGenerator/blob/main/DESCRIPTION)
[![R
version](https://img.shields.io/badge/R-%3E%3D%203.3.0-276DC3)](https://www.r-project.org/)
[![License: Apache
2.0](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![GitHub
issues](https://img.shields.io/github/issues/OHDSI/ProtocolGenerator)](https://github.com/OHDSI/ProtocolGenerator/issues)
[![Last
commit](https://img.shields.io/github/last-commit/OHDSI/ProtocolGenerator)](https://github.com/OHDSI/ProtocolGenerator/commits/main)

# Introduction

Create HTML protocol document using the Strategus json specification

# Examples

``` r

# install dependencies
remotes::install_github('ohdsi/ProtocolGenerator')

library(ProtocolGenerator)

# to run the protocol generator with a demo json specification
test <- generateProtocol(
  jsonLocation = getDemoLoc(),
  webAPI = paste0('https://', 'api.ohdsi.org', '/WebAPI'),
  outputLocation = './protocol'
)

# to run with your own json spec 
test <- generateProtocol(
  jsonLocation = '<insert location to json specification file>',
  webAPI = paste0('https://', 'api.ohdsi.org', '/WebAPI'),
  outputLocation = './protocol'
)
```

# Technology

ProtocolGenerator is an R package.

# System Requirements

Running the package requires R.

# Installation

1.  In R, use the following commands to download and install
    ProtocolGenerator:

``` r

install.packages("remotes")
remotes::install_github("ohdsi/ProtocolGenerator")
```

# User Documentation

Documentation is built with pkgdown and published by the pkgdown
workflow.

# Support

- Developer questions/comments/feedback: [OHDSI
  Forum](http://forums.ohdsi.org/c/developers)
- We use the [GitHub issue
  tracker](https://github.com/OHDSI/ProtocolGenerator/issues) for all
  bugs/issues/enhancements

# Contributing

Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can
contribute to this package.

# License

ProtocolGenerator is licensed under Apache License 2.0.

# Development

ProtocolGenerator is being developed in R Studio.

### Development status

Under development

# Acknowledgements
