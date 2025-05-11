<!-- DO NOT EDIT:  README.md is created from README.Rmd -->

<div>

<table style="border: 0">
<tr>
<td>
<img align="left" width="200" height="200" src="man/figures/logo.svg"/>
</td>
</tr>
</table>

</div>

<footer>
© Copyright 2024 Robert Carnell
</footer>

# triangle

An R package to work with the triangle distribution and logarithmic
triangle distribution

|                                                                            <sub>Github Actions</sub>                                                                             |                                                                                <sub>Windows</sub>                                                                                |                                                                   <sub>Code Coverage</sub>                                                                   |                                   <sub>CRAN Downloads</sub>                                    |                                               <sub>CRAN</sub>                                                |
|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------:|:----------------------------------------------------------------------------------------------:|:------------------------------------------------------------------------------------------------------------:|
| [![R-CMD-check](https://github.com/bertcarnell/triangle/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/bertcarnell/triangle/actions/workflows/r-cmd-check.yml) | [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/bertcarnell/triangle?branch=master&svg=true)](https://ci.appveyor.com/project/bertcarnell/triangle) | [![Coverage status](https://codecov.io/gh/bertcarnell/triangle/branch/master/graph/badge.svg)](https://codecov.io/github/bertcarnell/triangle?branch=master) | [![](https://cranlogs.r-pkg.org/badges/triangle)](https://cran.r-project.org/package=triangle) | [![CRAN status](https://www.r-pkg.org/badges/version/triangle)](https://cran.r-project.org/package=triangle) |

See the package documentation
[here:](https://bertcarnell.github.io/triangle/)

## Getting Started

Install the R package:

``` r
# Stable CRAN version
install.packages("triangle")

# OR development version from GitHub
require(devtools)
devtools::install_github("bertcarnell/triangle")
```

use the functions:

- `a` = minimum
- `b` = maximum
- `c` = mode

``` r
require(triangle)
```

### Triangle distribution

``` r
# rtriangle(n, a, b, c)
set.seed(42)
rtriangle(5, 1, 5, 2)
```

    ## [1] 3.988898 4.131038 2.073171 3.573596 2.926584

``` r
# ptriangle(x, a, b, c)
ptriangle(0:5, 0, 10, 5)
```

    ## [1] 0.00 0.02 0.08 0.18 0.32 0.50

``` r
# qtriangle(p, a, b, c)
qtriangle(seq(0, 1, by = 0.2), 1, 10, 3)
```

    ## [1]  1.000000  2.897367  3.851830  4.980040  6.450352 10.000000

``` r
# dtriangle(x, a, b, c)
dtriangle(0:4, 0, 10, 5)
```

    ## [1] 0.00 0.04 0.08 0.12 0.16

### Logarithmic triangle distribution

``` r
# rltriangle(n, a, b, c, logbase)
set.seed(2001)
rltriangle(5, 1, 100, 10)
```

    ## [1] 20.195183 13.001831  4.579489  4.753026  3.572658

``` r
# pltriangle(x, a, b, c, logbase)
pltriangle(10^(0:3), 1, 1000, 10)
```

    ## [1] 0.0000000 0.3333333 0.8333333 1.0000000

``` r
# qltriangle(p, a, b, c, logbase)
qltriangle(seq(0, 1, by = 0.2), 1, 100, 20)
```

    ## [1]   1.00000   5.26497  10.47630  17.76210  29.59642 100.00000

``` r
# dltriangle(x, a, b, c, logbase)
dltriangle(0:5, 1, 10, 5)
```

    ## [1] 0.0000000 0.0000000 0.8613531 1.3652124 1.7227062 2.0000000

### Parameter estimates

#### triangle method of moments estimates

``` r
x <- rtriangle(20, 0, 2, 1.5)
triangle_mom(x)
```

    ##         a         b         c 
    ## 0.6341961 1.9096262 1.4197678

#### triangle maximum likelihood estimates

``` r
x <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
# triangle_mle(x, debug = FALSE, maxiter = 100)
triangle_mle(x)
```

    ## Triangle Maximum Likelihood Estimates
    ## 
    ## Call:  triangle_mle(x = x) 
    ## 
    ## Estimates:
    ##    Estimate Std.Err
    ## a 0.0076277  0.0996
    ## b 0.9939370  0.1649
    ## c 0.3000000  0.0861
    ## 
    ## Convergence Code:  0
    ##   CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH

``` r
# standard triangle (0,1) likelihood estimates
standard_triangle_mle(x)
```

    ## Triangle Maximum Likelihood Estimates
    ## 
    ## Call:  standard_triangle_mle(x = x) 
    ## 
    ## Estimates:
    ##   Estimate Std.Err
    ## a      0.0  0.0000
    ## b      1.0  0.0000
    ## c      0.3  0.0871
    ## 
    ## Convergence Code:  NA
    ##  

``` r
set.seed(1976)
x <- rtriangle(100, 1, 5, 3.5)
triangle_mle(x)
```

    ## Triangle Maximum Likelihood Estimates
    ## 
    ## Call:  triangle_mle(x = x) 
    ## 
    ## Estimates:
    ##   Estimate Std.Err
    ## a   0.9060  0.1259
    ## b   4.8254  0.0770
    ## c   3.6853  0.0924
    ## 
    ## Convergence Code:  0
    ##   CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH
