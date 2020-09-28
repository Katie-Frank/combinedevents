
<!-- README.md is generated from README.Rmd. Please edit that file -->

# combinedevents

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/combinedevents)](https://CRAN.R-project.org/package=combinedevents)
[![R build
status](https://github.com/Katie-Frank/combinedevents/workflows/R-CMD-check/badge.svg)](https://github.com/Katie-Frank/combinedevents/actions)
[![codecov](https://codecov.io/gh/Katie-Frank/combinedevents/branch/master/graph/badge.svg)](https://codecov.io/gh/Katie-Frank/combinedevents)

<!-- badges: end -->

## Overview

**combinedevents** is an R package to calculate scores and marks for
track and field combined events competitions. The functions included in
this package are based on the scoring tables for combined events created
by the International Association of Athletics Federation (2001). These
scoring tables are available for download as a PDF:

  - Go to the [Technical Information Section of the World Athletics
    Website](https://www.worldathletics.org/about-iaaf/documents/technical-information)
  - Select *Scoring Tables*
  - Download *IAAF Scoring Tables for Combined Events*

## Installation

Version 0.1.0 is on [CRAN](https://CRAN.R-project.org) and can be
installed with:

``` r
install.packages("combinedevents")
```

The development version of **combinedevents** can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Katie-Frank/combinedevents")
```

## Usage

The two main functions in the package are

  - `scores()` calculates scores
  - `marks()` calculates marks

Below are a couple of quick examples. To learn more about
**combinedevents**, see the package documentation.

``` r
library(combinedevents)

# scores for men's decathlon
scores(c(10.8, 7.1, 12, 2.05, 49.5, 14.5, 35, 4.6, 40, "4:21"), "male", "decathlon")
#>    decathlon    mark score
#> 1       100m    10.8   906
#> 2         LJ     7.1   838
#> 3         SP      12   606
#> 4         HJ    2.05   850
#> 5       400m    49.5   838
#> 6      110mH    14.5   911
#> 7         DT      35   564
#> 8         PV     4.6   790
#> 9         JT      40   442
#> 10     1500m 4:21.00   805
#> 11     TOTAL    <NA>  7550

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

## References

International Association of Athletics Federation (2001). *IAAF Scoring
Tables for Combined Events*.
