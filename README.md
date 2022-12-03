<div>
  <table style="border: 0">
    <tr>
      <td>
        <img align="left" width="200" height="200" src="logo.svg"/>
      </td>
    </tr>
  </table>
</div>

An R package to work with the triangle distribution and logarithmic triangle distribution

|<sub>Github Actions</sub>|<sub>Windows</sub>|<sub>Code Coverage</sub>|<sub>CRAN Downloads</sub>|<sub>CRAN</sub>|
|:---:|:---:|:---:|:---:|:---:|
|[![R-CMD-check](https://github.com/bertcarnell/triangle/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/bertcarnell/triangle/actions/workflows/r-cmd-check.yml)|[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/bertcarnell/triangle?branch=master&svg=true)](https://ci.appveyor.com/project/bertcarnell/triangle)|[![Coverage status](https://codecov.io/gh/bertcarnell/triangle/branch/master/graph/badge.svg)](https://codecov.io/github/bertcarnell/triangle?branch=master)|[![](https://cranlogs.r-pkg.org/badges/triangle)](https://cran.r-project.org/package=triangle)|[![CRAN status](https://www.r-pkg.org/badges/version/triangle)](https://cran.r-project.org/package=triangle)|

See the package documentation [here:](https://bertcarnell.github.io/triangle/)

## Getting Started

Install the R package:

```r
# Stable CRAN version
install.packages(triangle)

# OR development version from GitHub
require(devtools)
devtools::install_github("bertcarnell/triangle")
```

use the functions:

- `a` = minimum
- `b` = maximum
- `c` = mode

```r
require(triangle)
```

### Triangle distribution

```r
# rtriangle(n, a, b, c)
rtriangle(5, 1, 5, 2)

# ptriangle(x, a, b, c)
ptriangle(0:5, 0, 10, 5)

# qtriangle(p, a, b, c)
qtriangle(seq(0, 1, by = 0.2), 1, 10, 3)

# dtriangle(x, a, b, c)
dtriangle(0:4, 0, 10, 5)
```

### Logarithmic triangle distribution

```r
# rltriangle(n, a, b, c, logbase)
rltriangle(5, 1, 100, 10)

# pltriangle(x, a, b, c, logbase)
pltriangle(10^(0:3), 1, 1000, 10)

# qltriangle(p, a, b, c, logbase)
qltriangle(seq(0, 1, by = 0.2), 1, 100, 20)

# dltriangle(x, a, b, c, logbase)
dltriangle(0:5, 1, 10, 5)
```

### Parameter estimates

```r
# triangle method of moments estimates
x <- rtriangle(20, 0, 2, 1.5)
triangle_mom(x)

# triangle maximum likelihood estimates
x <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
# triangle_mle(x, debug = FALSE, maxiter = 100)
triangle_mle(x)

# standard triangle (0,1) likelihood estimates
standard_triangle_mle(x)
```
