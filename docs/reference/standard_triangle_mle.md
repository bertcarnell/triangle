# Maximum likelihood estimate of the standard triangle distribution mode

Maximum likelihood estimate of the standard triangle distribution mode

## Usage

``` r
standard_triangle_mle(x, debug = FALSE)
```

## Arguments

- x:

  sample from a triangle distribution

- debug:

  if `TRUE` then the function will check the input parameters and print
  calculation information

## Value

an object of S3 class `triangle_mle` containing a list with the call,
coefficients, variance co-variance matrix, minimum negative log
likelihood, number of observations, and the sample

## References

Samuel Kotz and Johan Rene van Dorp. Beyond Beta
[doi:10.1142/5720](https://doi.org/10.1142/5720)

## Examples

``` r
xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
standard_triangle_mle(xtest)
#> Triangle Maximum Likelihood Estimates
#> 
#> Call:  standard_triangle_mle(x = xtest) 
#> 
#> Estimates:
#>   Estimate Std.Err
#> a      0.0  0.0000
#> b      1.0  0.0000
#> c      0.3  0.0871
#> 
#> Convergence Code:  NA
#>   

xtest <- rtriangle(20, 0, 1, 0.63)
standard_triangle_mle(xtest)
#> Triangle Maximum Likelihood Estimates
#> 
#> Call:  standard_triangle_mle(x = xtest) 
#> 
#> Estimates:
#>   Estimate Std.Err
#> a  0.00000  0.0000
#> b  1.00000  0.0000
#> c  0.67182  0.0556
#> 
#> Convergence Code:  NA
#>   
```
