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

|<sub>Linux & MacOS</sub>|<sub>Windows</sub>|<sub>Code Coverage</sub>|<sub>CRAN Downloads</sub>|<sub>CRAN</sub>|
|:---:|:---:|:---:|:---:|:---:|
|[![Travis build status](https://travis-ci.org/bertcarnell/triangle.svg?branch=master)](https://travis-ci.org/bertcarnell/triangle)|[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/bertcarnell/triangle?branch=master&svg=true)](https://ci.appveyor.com/project/bertcarnell/triangle)|[![Coverage status](https://codecov.io/gh/bertcarnell/triangle/branch/master/graph/badge.svg)](https://codecov.io/github/bertcarnell/triangle?branch=master)|[![](https://cranlogs.r-pkg.org/badges/triangle)](https://cran.r-project.org/package=triangle)|[![CRAN status](https://www.r-pkg.org/badges/version/triangle)](https://cran.r-project.org/package=triangle)|

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

# triangle distribution

# rtriangle(n, a, b, c)
rtriangle(5, 1, 5, 2)

# ptriangle(x, a, b, c)
ptriangle(0:5, 0, 10, 5)

# qtriangle(p, a, b, c)
qtriangle(seq(0, 1, by = 0.2), 1, 10, 3)

# dtriangle(x, a, b, c)
dtriangle(0:4, 0, 10, 5)

# logarithmic triangle distribution

# rlriangle(n, a, b, c, logbase)
rltriangle(5, 1, 100, 10)

# pltriangle(x, a, b, c, logbase)
pltriangle(10^(0:3), 1, 1000, 10)

# qltriangle(p, a, b, c, logbase)
qltriangle(seq(0, 1, by = 0.2), 1, 100, 20)

# dltriangle(x, a, b, c)
dltriangle(0:5, 1, 10, 5)
```
