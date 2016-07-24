
<!-- README.md is generated from README.Rmd. Please edit that file -->
Event study methodology for abnormal returns
============================================

[![Build Status](https://travis-ci.com/axelperschmann/AbnormalReturns.svg?token=Kjpsd3qrCAyMxmV9vstj&branch=master)](https://travis-ci.com/axelperschmann/AbnormalReturns) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/AbnormalReturns)](https://cran.r-project.org/package=AbnormalReturns) [![codecov](https://codecov.io/gh/axelperschmann/AbnormalReturns/branch/master/graph/badge.svg?token=2aoNCDfAgT)](https://codecov.io/gh/axelperschmann/AbnormalReturns)

**AbnormalReturns** implements the event study methodology and abnormal returns in particular. The event study methodology is a common way to study the effects of certain events on stock prices. It thus calculates a so-called abnormal return that measures the impact without confounding influences. As part of this method, one first has to predict a normal return in the absence of the event under study. Afterwards, one calculates the difference between the actual return and the previously predicted normal return, i.e. the abnormal return.

Overview
--------

**AbnormalReturns** provides a function `computeAbnormalReturn()`, that returns a data frame including the abnormal returns, the \(R^2\)-value of the responsible model for each analyzed date.

To see example function calls, check out the help pages and the vignette.

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

This section shows the basic functionality of how to compute the abnormal returns for an arbitrary commodity and it's corresponding market portfolio. First, load the corresponding package **AbnormalReturns**.

``` r
library(AbnormalReturns)
```

### Quick demonstration

This simple example computes the abnormal returns for commodity Adidas and market portfolio DAX for the year 2015, based on a `estimationWindowLength` of 20. The result is a data-frame with one row per abnormal return and five columns describing Date, abnormalReturn, R.squared, commodityReturn and portfolioReturn.

It also shows a visualization of the commodity performance and the corresponding abnormal returns.

``` r
abnormal = computeAbnormalReturn(portfolio=d.DAX, commodity=d.Adidas, regressionType='OLS',
                                 eventIndex=NULL, estimationWindowLength=20,
                                 attributeOfInterest='Close', showPlot=TRUE)
```

![](README-unnamed-chunk-4-1.png)

``` r
head(abnormal)
#>          Date abnormalReturn R.squared commodityReturn portfolioReturn
#> 21 2015-01-30      0.4713908 0.8986916           61.19        10694.32
#> 22 2015-02-02      0.3299405 0.9057373           61.79        10828.01
#> 23 2015-02-03      1.4813249 0.9061129           63.33        10890.95
#> 24 2015-02-04      1.6381956 0.8960916           63.83        10911.32
#> 25 2015-02-05      1.2093413 0.8893401           63.65        10905.41
#> 26 2015-02-06      1.1372783 0.8958292           63.39        10846.39

summary(abnormal$R.squared)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 0.0000006 0.4211000 0.6432000 0.5959000 0.8299000 0.9614000

summary(abnormal$abnormalReturn)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -3.4720 -0.4255  0.6554  0.8161  1.7180  7.0700
```

Further information
-------------------

-   Konchitchki, Y., and O'Leary, D. E. 2011. "Event study methodologies in information systems research," *International Journal of Accounting Information Systems* (12:2), pp. 99-115.
-   MacKinlay, A. C. 1997. "Event Studies in Economics and Finance," *Journal of Economic Literature* (35:1), pp. 13-39.

License
-------

**AbnormalReturns** is released under the [MIT License](https://opensource.org/licenses/MIT)

Copyright (c) 2016 Axel Perschmann, Stefan Feuerriegel & Nicolas Pröllochs
