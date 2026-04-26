# Triangle distribution method of moments estimate

Triangle distribution method of moments estimate

## Usage

``` r
triangle_mom(x, na.rm = FALSE, type = 1)
```

## Arguments

- x:

  triangle distribution sample

- na.rm:

  whether to remove NA samples. Default is FALSE.

- type:

  the type of method of moments. Type 1 uses the min and max. Type2
  minimizes the distance from the calculated mean, variance, and
  skewness to the sample mean, variance, and skewness

## Value

a named vector of the parameter estimates

## Examples

``` r
set.seed(1204)
x <- rtriangle(20, 0, 2, 1.5)
triangle_mom(x)
#>         a         b         c 
#> 0.6188926 1.9317298 1.2443817 
```
