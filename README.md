
<!-- README.md is generated from README.Rmd. Please edit that file -->
Event study methodology for abnormal returns
============================================

[![Build Status](https://travis-ci.org/axelperschmann/AbnormalReturns.svg?branch=master)](https://travis-ci.org/axelperschmann/AbnormalReturns) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/AbnormalReturns)](https://cran.r-project.org/package=AbnormalReturns) [![Coverage Status](https://img.shields.io/codecov/c/github/axelperschmann/AbnormalReturns/master.svg)](https://codecov.io/github/axelperschmann/AbnormalReturns?branch=master)

**AbnormalReturns** performs a **sentiment analysis** of textual contents in R. This implementation utilizes various existing dictionaries, such as General Inquirer, Harvard IV or Loughran-McDonald. Furthermore, it can also create customized dictionaries. The latter uses LASSO regularization as a statistical approach to select relevant terms based on an exogeneous response variable.

Overview
--------

The most important functions in **SentimentAnalysis** are:

-   Compute sentiment scores from contents stored in different formats with `analyzeSentiment()`.

-   If desired, convert the continuous scores to either binary sentiment classes (negative or positive) or tertiary directions (negative, neutral or positive). This conversion can be done with `convertToBinary()` or `convertToDirection()` respectively.

-   Compare the calculated sentiment socres with a baseline (i.e. a gold standard). Here, `compareToResponse()` performs a statistical evaluation, while `plotSentimentResponse()` enables a visual comparison.

-   Generate customized dictionaries with the help of `generateDictionary()` as part of an advanced analysis. However, this prerequisites a response variable (i.e. the baseline).

To see examples of these functions in use, check out the help pages, the demos and the vignette.

Installation
------------

Using the **devtools** package, you can easily install the latest development version of **AbnormalReturns** with

``` r
install.packages("devtools")

# Option 1: download and install latest version from ‘GitHub’
devtools::install_github("axelperschmann/AbnormalReturns")

# Option 2: install directly from bundled archive
# devtoos::install_local("AbnormalReturns_0.1-0.tar.gz")
```

Notes:

-   In the case of option 2, you have to specify the path either to the directory of **AbnormalReturns** or to the bundled archive **AbnormalReturns\_0.1-0.tar.gz**

-   A CRAN version has not yet been released.

Usage
-----

This section shows the basic functionality of how to perform a sentiment analysis. First, load the corresponding package **SentimentAnalysis**.

``` r
library(AbnormalReturns)
```

### Quick demonstration

This simple example shows how to perform a sentiment analysis of a single string. The result is a two-level factor with levels "positive" and "negative."

``` r
# Code
```

### Small example

The following demonstrates some of the functionality provided by **AbnormalReturns**. It also shows its visualization and evaluation capabilities.

Further information
-------------------

The approach utilizes LASSO regularization to extract words from documents that statistically feature a positive and negative polarity. This immediately reveals manifold implications for practitioners, finance research and social sciences: researchers can use R to extract text components that are relevant for readers and test their hypothesis based on these.

-   Proellochs, Feuerriegel and Neumann (2015): Generating Domain-Specific Dictionaries Using Bayesian Learning, Proceedings of the 23rd European Conference on Information Systems (ECIS 2015), Muenster, Germany. [DOI: 10.2139/ssrn.2522884](https://dx.doi.org/10.2139/ssrn.2522884)

License
-------

**AbnormalReturns** is released under the [MIT License](https://opensource.org/licenses/MIT)

Copyright (c) 2016 Axel Perschmann, Stefan Feuerriegel & Nicolas Pröllochs
