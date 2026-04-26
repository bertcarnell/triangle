# Maximum likelihood estimate of the triangle distribution parameters

Maximum likelihood estimate of the triangle distribution parameters

## Usage

``` r
triangle_mle(x, debug = FALSE, maxiter = 100, boot_var = FALSE, boot_rep = 500)
```

## Arguments

- x:

  sample from a triangle distribution

- debug:

  if `TRUE` then the function will check the input parameters and print
  calculation information

- maxiter:

  the maximum number of cycles of optimization between maximizing `a`
  and `b` given `c` and maximizing `c` given `a` and `b`

- boot_var:

  should the variance be computed with a bootstrap sample?

- boot_rep:

  The number of bootstrap replications

## Value

an object of S3 class `triangle_mle` containing a list with the call,
coefficients, variance co-variance matrix, minimum negative log
likelihood, details of the optimization number of observations, and the
sample

## References

Samuel Kotz and Johan Rene van Dorp. Beyond Beta
[doi:10.1142/5720](https://doi.org/10.1142/5720)

## Examples

``` r
xtest <- c(0.1, 0.25, 0.3, 0.4, 0.45, 0.6, 0.75, 0.8)
triangle_mle(xtest)
#> Triangle Maximum Likelihood Estimates
#> 
#> Call:  triangle_mle(x = xtest) 
#> 
#> Estimates:
#>   Estimate Std.Err
#> a -0.10366      NA
#> b  0.80000      NA
#> c  0.80000      NA
#> 
#> Convergence Code:  0
#>   CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH

xtest <- rtriangle(20, 1, 5, 3.5)
triangle_mle(xtest)
#> Triangle Maximum Likelihood Estimates
#> 
#> Call:  triangle_mle(x = xtest) 
#> 
#> Estimates:
#>   Estimate Std.Err
#> a   1.4791      NA
#> b   4.3481      NA
#> c   4.3481      NA
#> 
#> Convergence Code:  0
#>   CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH
```
