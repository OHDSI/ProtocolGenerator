ProtocolGenerator
===============

[![Build Status](https://github.com/OHDSI/ProtocolGenerator/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/ProtocolGenerator/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/ProtocolGenerator/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/ProtocolGenerator?branch=main)


Introduction
============

Create HTML protocol document using the Strategus json specification

Examples
========


```{r}
# install dependencies
remotes::install_github('ohdsi/ProtocolGenerator')

library(ProtocolGenerator)

# to run the protocol generator with a demo json specification
test <- generateProtocol(
  jsonLocation = getDemoLoc(),
  webAPI = 'https://api.ohdsi.org/WebAPI',
  outputLocation = './protocol'
)

# to run with your own json spec 
test <- generateProtocol(
  jsonLocation = '<insert location to json specification file>',
  webAPI = 'https://api.ohdsi.org/WebAPI',
  outputLocation = './protocol'
)


```


Technology
==========

ProtocolGenerator is an R package.


System Requirements
===================

Running the package requires R.


Installation
============

1. In R, use the following commands to download and install ProtocolGenerator:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/ProtocolGenerator")
  ```
  
User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/ProtocolGenerator/).


Support
=======

* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/ProtocolGenerator/issues">GitHub issue tracker</a> for all bugs/issues/enhancements


Contributing
============

Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.


License
=======

ProtocolGenerator is licensed under Apache License 2.0. 


Development
===========

ProtocolGenerator is being developed in R Studio.


### Development status

Under development


Acknowledgements
================

