# Quantile-Quantile Plot for Triangle Distributed Data

Quantile-Quantile Plot for Triangle Distributed Data

## Usage

``` r
qqtriangle(
  y,
  a,
  b,
  c,
  main = "Triangle Q-Q Plot",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles",
  ...
)
```

## Arguments

- y:

  the triangle distributed sample

- a:

  the theoretical distribution triangle minimum parameter

- b:

  the theoretical distribution triangle maximum parameter

- c:

  the theoretical distribution triangle mode parameter

- main:

  the plot title

- xlab:

  the x-axis label

- ylab:

  the y-axis label

- ...:

  other parameters passed to `qqplot`

## Value

a list of x-y coordinates on the plot

## Examples

``` r
set.seed(10304)
xtest <- rtriangle(100, 1, 5, 2)
theta <- coef(triangle_mle(xtest))
qqtriangle(xtest, theta[1], theta[2], theta[3])
```
