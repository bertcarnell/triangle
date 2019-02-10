# triangle
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

# rtriangle(n, a, b, c)
rtriangle(10, 1, 5, 2)

# ptriangle(x, a, b, c)
ptriangle(0:10, 0, 10, 5)

# qtriangle(p, a, b, c)
qtriangle(seq(0, 1, by = 0.1), 1, 10, 3)

# dtriangle(x, a, b, c)
dtriangle(0:10, 0, 10, 5)

```
