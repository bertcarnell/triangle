# Triangle parameter estimates using a non-linear fit of the empirical CDF

Triangle parameter estimates using a non-linear fit of the empirical CDF

## Usage

``` r
triangle_cdfe(x, control = stats::nls.control(maxiter = 100, warnOnly = TRUE))
```

## Arguments

- x:

  the triangle distributed sample

- control:

  an object created by
  [`stats::nls.control`](https://rdrr.io/r/stats/nls.control.html)

## Value

an object of class `nls`

## Examples

``` r
set.seed(10304)
xtest <- rtriangle(100, 1, 5, 2)
cdfe <- triangle_cdfe(xtest)
print(cdfe)
#> Nonlinear regression model
#>   model: ecdf1 ~ ptriangle(x, a, b, c)
#>    data: parent.frame()
#>      a      b      c 
#> 0.7848 5.3555 2.1059 
#>  residual sum-of-squares: 0.03265
#> 
#> Algorithm "port", convergence message: relative convergence (4)
summary(cdfe)
#> 
#> Formula: ecdf1 ~ ptriangle(x, a, b, c)
#> 
#> Parameters:
#>   Estimate Std. Error t value Pr(>|t|)    
#> a  0.78482    0.04266   18.40   <2e-16 ***
#> b  5.35551    0.03210  166.84   <2e-16 ***
#> c  2.10587    0.04613   45.65   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.01835 on 97 degrees of freedom
#> 
#> Algorithm "port", convergence message: relative convergence (4)
#> 
coef(cdfe)
#>         a         b         c 
#> 0.7848163 5.3555055 2.1058729 
if (FALSE) { # \dontrun{
  confint(cdfe)
} # }
```
