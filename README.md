# dssd
Distance Sampling Survey Design
===============================
<!---
[![R-CMD-check](https://github.com/DistanceDevelopment/dssd/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/DistanceDevelopment/dssd/actions/workflows/check-standard.yaml)
-->
[![CRAN (RStudio Mirror) Downloads](http://cranlogs.r-pkg.org/badges/dssd)](https://www.r-pkg.org/pkg/dssd)
[![CRAN Version](http://www.r-pkg.org/badges/version/dssd)](https://www.r-pkg.org/pkg/dssd)
[![Codecov test coverage](https://codecov.io/gh/DistanceDevelopment/dssd/branch/master/graph/badge.svg)](https://app.codecov.io/gh/DistanceDevelopment/dssd?branch=master)

`dssd` is a package for designing distance sampling surveys. It provides a number of designs including systematic point transect, parallel line transect, zigzag line transect and segment line transect designs.

# Using `dssd`

There are two vignettes within the dssd package to help you get started using `dssd` (see Articles on navbar):

  - [Getting Started](articles/GettingStarted.html)
  - [Multiple Strata](articles/MultiStrataVignette.html)

# Getting `dssd`

The easiest way to get `dssd` is to install it from CRAN within R-studio or the R interface. We endeavour to make all new functionality available on CRAN in a timely manor. However, if you wish to download the development version with the latest updates immediately you can do this using Hadley Wickham's `devtools` package:

      # First, ensure you have a copy of the `devtools` package:
      if (!nzchar(system.file(package = "devtools"))) install.packages("devtools")

then install `dssd` from github:

      library(devtools)
      install_github("DistanceDevelopment/dssd", build_vignettes = TRUE)

### Troubleshooting tip

During installation of packages, you may get the message "These packages have more recent versions available. It is recommended to update all of them. Which would you like to update?" and then a list of packages. We recommend you typically choose the option "CRAN packages only".  Note you may then get the message that some packages cannot be installed because they are already loaded.  In this case, a solution may be to note which packages these are, to open an R console (rather than R Studio) and to use the `Packages | Update packages` menu option (or the `update.packages` function) to update these packages.

<!-- The easiest way to get `dssd` is to install it from CRAN within R-studio or the R interface. We endeavour to make all new functionality available on CRAN in a timely manor. However, if you wish to download the development version with the latest updates immediately you can do this using Hadley Wickham's `devtools` package:

      install.packages("devtools")

then install `dssd` from github:

      library(devtools)
      install_github("DistanceDevelopment/dssd", build_vignettes = TRUE)
-->
