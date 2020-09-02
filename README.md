
<!-- README.md is generated from README.Rmd. Please edit that file -->

# combinedevents

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/combinedevents)](https://CRAN.R-project.org/package=combinedevents)
[![Build
Status](https://travis-ci.org/Katie-Frank/combinedevents.svg?branch=master)](https://travis-ci.org/Katie-Frank/combinedevents)
[![Codecov test
coverage](https://codecov.io/gh/Katie-Frank/combinedevents/branch/master/graph/badge.svg)](https://codecov.io/gh/Katie-Frank/combinedevents?branch=master)
<!-- badges: end -->

## Overview

The **combinedevents** package provides functions to calculate scores
and marks for track and field combined events competitions. The
functions are based on the IAAF Scoring Tables for Combined Events.

## Installation

You can install the released version of **combinedevents** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("combinedevents")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Katie-Frank/combinedevents")
```

## Usage

The two main functions in the package are

  - `scores()` calculates scores
  - `marks()` calculates marks

Below are some quick examples. To learn more about **combinedevents**,
see package documentation.

``` r
library(combinedevents)

# scores for men's decathlon

# marks for women's heptathlon
marks(c(900, 1014, 790, 1000, 788, 800, 1000), "female", "heptathlon")
#>   heptathlon    mark score
#> 1      100mH   14.56   901
#> 2         HJ    1.83  1016
#> 3         SP   13.94   790
#> 4       200m    23.8  1000
#> 5         LJ     5.8   789
#> 6         JT   46.87   800
#> 7       800m 2:07.63  1000
#> 8      TOTAL    <NA>  6296
```
