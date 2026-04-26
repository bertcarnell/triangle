# The Log-Triangle Distribution

These functions provide information about the triangle distribution on
the logarithmic interval from `a` to `b` with a maximum at `c`.
`dltriangle` gives the density, `pltriangle` gives the distribution
function, `qltriangle` gives the quantile function, and `rltriangle`
generates `n` random deviates.

## Usage

``` r
rltriangle(
  n = 1,
  a = 1,
  b = 100,
  c = 10^((log10(a) + log10(b))/2),
  logbase = 10
)

dltriangle(x, a = 1, b = 100, c = 10^((log10(a) + log10(b))/2), logbase = 10)

pltriangle(q, a = 1, b = 100, c = 10^((log10(a) + log10(b))/2), logbase = 10)

qltriangle(p, a = 1, b = 100, c = 10^((log10(a) + log10(b))/2), logbase = 10)
```

## Arguments

- n:

  number of observations. If `length(n) > 1`, the length is taken to be
  the number required.

- a:

  lower limit of the distribution.

- b:

  upper limit of the distribution.

- c:

  mode of the distribution.

- logbase:

  the base of the logarithmic scale to use (default to 10)

- x, q:

  vector of quantiles.

- p:

  vector of probabilities.

## Value

`dltriangle` gives the density, `pltriangle` gives the distribution
function, `qltriangle` gives the quantile function, and `rltraingle`
generates random deviates. Invalid arguments will result in return value
`NaN` or `NA`.

## Details

All probabilities are lower tailed probabilties. `a`, `b`, and `c` may
be appropriate length vectors except in the case of `rtriangle`.

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`.Random.seed`](https://rdrr.io/r/base/Random.html) about random number
generation, [`runif`](https://rdrr.io/r/stats/Uniform.html), etc for
other distributions.

## Examples

``` r
tri <- rltriangle(100000, 1, 100, 10)
hist(log10(tri), breaks=100, main="Triangle Distribution", xlab="x")

dltriangle(10, 1, 100, 10) # 2/(log10(b)-log10(a)) = 1
#> [1] 1
qltriangle(pltriangle(10)) # 10
#> [1] 10
```
