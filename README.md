# dssd
Distance Sampling Survey Design
===============================

[![R-CMD-check](https://github.com/DistanceDevelopment/dssd/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/DistanceDevelopment/dssd/actions/workflows/check-standard.yaml)
[![CRAN (RStudio Mirror) Downloads](http://cranlogs.r-pkg.org/badges/dssd)](https://www.r-pkg.org/pkg/dssd)
[![CRAN Version](http://www.r-pkg.org/badges/version/dssd)](https://www.r-pkg.org/pkg/dssd)
[![Codecov test coverage](https://codecov.io/gh/DistanceDevelopment/dssd/branch/master/graph/badge.svg)](https://app.codecov.io/gh/DistanceDevelopment/dssd?branch=master)

`dssd` is a package for designing distance sampling surveys. It provides a number of designs including systematic point transect, parallel line transect, zigzag line transect and segment line transect designs.

# Using `dssd`

There are two vignettes within the dssd package to help you get started using `dssd`:
  - GettingStarted: "Getting Started with dssd"
  - MultiStrataVignette: "Multiple Strata in dssd"

# Getting `dssd`

We typically aim to keep `dssd` on CRAN, so it can be readily installed from within R-Studio or the R interface.  However, at present there is an [issue](https://github.com/DistanceDevelopment/dssd/issues/94) that prevents this.  Therefore to obtain `dssd` at present, please use the following code.  

First, ensure you have a copy of the `devtools` package:

     if (system.file(package = "devtools") == "") install.packages("devtools")

then install `dssd` from github:

      library(devtools)
      install_github("DistanceDevelopment/dssd", build_vignettes = TRUE)

<!-- The easiest way to get `dssd` is to install it from CRAN within R-studio or the R interface. We endeavour to make all new functionality available on CRAN in a timely manor. However, if you wish to download the development version with the latest updates immediately you can do this using Hadley Wickham's `devtools` package:

      install.packages("devtools")

then install `dssd` from github:

      library(devtools)
      install_github("DistanceDevelopment/dssd", build_vignettes = TRUE)
-->
