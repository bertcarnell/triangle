# Utility Methods for S3 class triangle_mle

Utility Methods for S3 class triangle_mle

## Usage

``` r
# S3 method for class 'triangle_mle'
summary(object, ...)

# S3 method for class 'triangle_mle'
print(x, ...)

# S3 method for class 'triangle_mle'
coef(object, ...)

# S3 method for class 'triangle_mle'
logLik(object, ...)

# S3 method for class 'triangle_mle'
AIC(object, ..., k = 2)

# S3 method for class 'triangle_mle'
BIC(object, ...)

# S3 method for class 'triangle_mle'
vcov(object, ...)

# S3 method for class 'triangle_mle'
profile(fitted, ...)

# S3 method for class 'triangle_mle'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  class triangle_mle from a call to
  [`triangle_mle()`](https://bertcarnell.github.io/triangle/reference/triangle_mle.md)

- ...:

  not used except for `print` (other arguments passed to `printCoefmat`)

- x:

  the `triangle_mle` object

- k:

  the penalty per parameter to be used; the default `k = 2`

- fitted:

  an object of class triangle_mle

- parm:

  parameters to be given confidence intervals passed to
  `stats4::confint`

- level:

  confidence interval level passed to `stats4::confint`

## Value

an object of class summary.mle

`print.triangle_mle`: `x` invisibly

`coef.triangle_mle`: a vector of coefficients

`logLik.triangle_mle`: an object of class `logLik`

`AIC.triangle_mle`: the AIC

`BIC.triangle_mle`: the BIC

`vcov.triangle_mle`: the variance co-variance matrix

`profile.triangle_mle`: an object of class `profile.mle`

`confint.triangle_mle`: a matrix of parameter confidence intervals

## Examples

``` r
set.seed(1234)
x <- rtriangle(100, 0, 1, 0.5)
mle1 <- triangle_mle(x)
summary(mle1)
#> Maximum likelihood estimation
#> 
#> Call:
#> triangle_mle(x = x)
#> 
#> Coefficients:
#>     Estimate Std. Error
#> a 0.02332693 0.02638940
#> b 0.96376879 0.02408262
#> c 0.38903301 0.02333772
#> 
#> -2 log L: -51.57153 
print(mle1)
#> Triangle Maximum Likelihood Estimates
#> 
#> Call:  triangle_mle(x = x) 
#> 
#> Estimates:
#>   Estimate Std.Err
#> a 0.023327  0.0264
#> b 0.963769  0.0241
#> c 0.389033  0.0233
#> 
#> Convergence Code:  0
#>   CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH
coef(mle1)
#>          a          b          c 
#> 0.02332693 0.96376879 0.38903301 
logLik(mle1)
#> 'log Lik.' 25.78577 (df=3)
AIC(mle1)
#> [1] -45.57153
BIC(mle1)
#> [1] -37.75602
vcov(mle1)
#>               a             b            c
#> a  6.964006e-04 -4.543362e-05 0.0000000000
#> b -4.543362e-05  5.799725e-04 0.0000000000
#> c  0.000000e+00  0.000000e+00 0.0005446493
if (FALSE) { # \dontrun{
  prof <- profile(mle1)
  stats4::plot(prof)
  confint(mle1, 1:3, level = 0.95)
} # }
```
