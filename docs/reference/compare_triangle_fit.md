# Compare multiple triangle distributions fits

Compare multiple triangle distributions fits

## Usage

``` r
compare_triangle_fit(
  y,
  cols = c("red", "blue", "green"),
  main = "Triangle Fit Comparison",
  ...
)
```

## Arguments

- y:

  the triangle distributed sample

- cols:

  the colors of the CDF-based estimates, the maximum likelihood
  estimates, and the method of moments estimates

- main:

  the plot title

- ...:

  other parameters passed to `plot.ecdf`

## Examples

``` r
set.seed(10304)
xtest <- rtriangle(100, 1, 5, 2)
compare_triangle_fit(xtest)
```
