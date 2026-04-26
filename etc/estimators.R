require(triangle)

X <- rtriangle(100, 1, 6, 2)

method_of_moments_triangle <- function(x)
{
  ap <- min(X)
  bp <- max(X)
  cp <- 3*mean(X) - ap - bp
  return(c(ap, bp, cp))
}

method_of_moments_triangle(X)

# https://www.worldscientific.com/doi/suppl/10.1142/5720/suppl_file/5720_chap1.pdf

# 1.43
M <- function(z, ak, bk, rk)
{
  s <- length(z)
  stopifnot(rk <= s & rk >= 1)
  stopifnot(ak < bk)
  stopifnot(ak <= min(z))
  stopifnot(bk >= max(z))
  stopifnot(length(rk) == 1)
  stopifnot(all(order(z) == 1:s))
  if (rk > 1 & rk < s)
  {
    #return(prod((z[1:(rk-1)] - ak) / (z[rk] - ak)) * prod((bk - z[(rk+1):s]) / (bk - z[rk])))
    return(sum(log(z[1:(rk-1)] - ak)) - rk*log(z[rk] - ak) + sum(log(bk - z[(rk+1):s])) - (s-rk)*log(bk - z[rk]))
  } else if (rk == 1)
  {
    #return(prod((bk - z[(rk+1):s]) / (bk - z[rk])))
    return(sum(log(bk - z[(rk+1):s])) - (s-rk)*log(bk - z[rk]))
  } else if (rk == s)
  {
    #return(prod((z[1:(rk-1)] - ak) / (z[rk] - ak)))
    return(sum(log(z[1:(rk-1)] - ak)) - (s-1)*log(z[rk] - ak))
  }
}

M(sort(X), 0, 7, 1)
M(sort(X), 0, 7, 2)
M(sort(X), 0, 7, 33)
M(sort(X), 0, 7, 34)
M(sort(X), 0, 7, 35)
M(sort(X), 0, 7, 99)
M(sort(X), 0, 7, 100)

# 1.42
rhat_fun <- function(z, a, b)
{
  s <- length(z)
  stopifnot(a < b)
  stopifnot(a <= min(z))
  stopifnot(b >= max(z))
  stopifnot(all(order(z) == 1:s))
  res <- sapply(1:s, function(r) M(z, a, b, r))
  return(which.max(res))
}

rhat_fun(sort(X), 0, 7)

sort(X)[rhat_fun(sort(X), 0, 7)]

# 1.48
G <- function(z, ak, bk)
{
  rhat <- rhat_fun(z, ak, bk)
  s <- length(z)
  stopifnot(rhat <= s & rhat >= 1)
  stopifnot(ak < bk)
  stopifnot(ak <= min(z))
  stopifnot(bk >= max(z))
  log(M(z, ak, bk, rhat)) - s*log(bk-ak)
}

# 1.52
dG_da <- function(z, ak, bk)
{
  s <- length(z)
  rhat <- rhat_fun(z, ak, bk)
  dM_da(z, ak, bk, rhat) / M(z, ak, bk, rhat) + s / (bk - ak)
}

# 1.53
dG_db <- function(z, ak, bk)
{
  s <- length(z)
  rhat <- rhat_fun(z, ak, bk)
  dM_db(z, ak, bk, rhat) / M(z, ak, bk, rhat) - s / (bk - ak)
}

# 1.54
dM_da <- function(z, ak, bk, rhat)
{
  M(z, ak, bk, rhat) * sum((z[1:(rhat-1)] - z[rhat])/(z[rhat] - ak)/(z[1:(rhat-1)] - ak))
}

# 1.55
dM_db <- function(z, ak, bk, rhat)
{
  s <- length(z)
  M(z, ak, bk, rhat) * sum((z[(rhat+1):s] - z[rhat])/(bk - z[rhat])/(bk-z[(rhat+1):s]))
}

sX <- sort(X)

M(sX, 0, 7, 5)
G(sX, 0, 7)
dG_da(sX, 0, 7)
dG_db(sX, 0, 7)
dM_da(sX, 0, 7, 5)
dM_db(sX, 0, 7, 5)
rhat_fun(sX, 0, 7)

fb <- function(z, a, r)
{
  s <- length(z)
  stopifnot(r >= 1)
  stopifnot(r <= s)
  (z[r+1] - z[r]*((z[r]-a)/(z[r+1]-a))^(r/(s-r)))/(1-((z[r]-a)/(z[r+1]-a))^(r/(s-r)))
}

fb(sX,0,5)

B <- function(z, a)
{
  s <- length(z)
  res <- sapply(1:(s-1), function(r) fb(z, a, r))
  return(max(res))
}

B(sX, 0)

BSearch <- function(ak, z)
{
  lkb <- max(z)
  bfirst <- TRUE
  delta <- 1E-3
  maxiter <- 1000
  iter <- 1
  while (bfirst || ukb-lkb >= delta)
  {
    if (iter > maxiter)
    {
      print("Max iteations reached")
      break
    }
    bfirst <- FALSE
    ukb <- B(z, ak)
    bk <- (lkb + ukb) / 2
    rk <- rhat_fun(z, ak, bk)
    Mk <- M(z, ak, bk, rk)
    Gk <- dG_db(z, ak, bk, rk)
    if (abs(Gk) >= delta)
    {
      if (Gk < 0)
      {
        ukb <- bk
      } else {
        lkb <- bk
      }
    } else
    {
      return(list(bk=bk, Mk=Mk, rk=rk))
    }
    iter <- iter + 1
  }
  return(list(bk=bk, Mk=Mk, rk=rk))
}

#BSearch(0, sX)


#f <- function(b, z, a) -1*G(z, a, b)

#o <- optimize(f, lower = max(sX), upper = B(sX, 0), z=sX, a = 0)
#bk <- o$minimum



ml_triangle <- function(x)
{
  # x <- X
  f <- function(par, z)
  {
    res <- -1*G(z, par[1], par[2])
    if (is.finite(res) & !is.na(res))
    {
      return(res)
    } else
    {
      if (!is.na(res))
      {
        res <- sign(res)*1E100
      } else
      {
        res <- c(1E100,1E100)
      }
      return(res)
    }
  }
  g <- function(par, z)
  {
    res <- -1*c(dG_da(z, par[1], par[2]),
                dG_db(z, par[1], par[2]))
    if (all(is.finite(res)) & all(!is.na(res)))
    {
      return(res)
    } else if (all(!is.na(res)))
    {
      res <- sign(res)*1E100
    } else
    {
      res <- c(1E100,1E100)
    }
    return(res)
  }
  sx <- sort(x)
  # upper a is 1st order stat
  ua <- min(x)
  # lower a is lst order stat minus the range
  la <- min(x) - (max(x) - min(x))
  ub <- B(sx, la)
  lb <- max(x)
  dist <- max(x) - min(x)
  o <- optim(par=c((la+ua)/2, (lb+ub)/2),
          fn=f, gr=g, z = sx,
          method = "L-BFGS-B",
          lower = c(la, lb + dist/1000),
          upper = c(ua - dist/1000, ub))
  rhat <- rhat_fun(sx, o$par[1], o$par[2])
  return(list(a=o$par[1], b=o$par[2], c=sx[rhat]))
}


ml_triangle(X)






require(triangle)
require(stats4)

# test data
set.seed(18983)
y <- triangle::rtriangle(1000, 1, 6, 2)

# negative log likelihood
nLL <- function(a, b, c)
{
  res <- -1*sum(log(triangle::dtriangle(y, a, b, c)))
  if (is.finite(res) & !is.na(res))
    return(res)
  else if (!is.na(res))
    return(sign(res)*1E100) # something large
  else
    return(1E100)
}

rangey <- max(y) - min(y)
mle1 <- stats4::mle(nLL, start = c(min(y)-0.1*rangey, max(y) + 0.1*rangey, median(y)),
                    lower = c(min(y) - 2*rangey, max(y), min(y)),
                    upper = c(min(y), max(y) + 2*rangey, max(y)),
                    nobs = length(y))

# maximum likelihood estimate
coef(mle1)
# AIC
AIC(mle1)
# BIC
BIC(mle1)
# Kolmogorov-Smirinov test
ks.test(y, "ptriangle", a = coef(mle1)['a'], b = coef(mle1)['b'], c = coef(mle1)['c'])
# Log likelihood
-1 * nLL(a = coef(mle1)['a'], b = coef(mle1)['b'], c = coef(mle1)['c'])

# Method of moments estimates
c(a = min(y), b = max(y), c = 3*mean(y) - min(y) - max(y))

triangle_mom <- function(x, na.rm = FALSE)
{
  minx <- min(x, na.rm = na.rm)
  maxx <- max(x, na.rm = na.rm)
  c(a = minx,
    b = maxx,
    c = 3*mean(x, na.rm = na.rm) - minx - maxx)
}

nLL_triangle <- function(x, a, b, c)
{
  -1*sum(log(triangle::dtriangle(x, a, b, c)))
}

triangle::dtriangle(c(0.5, 3), 1, 2, 1)
triangle::dtriangle(c(0.5, 3, 1, 2), 1, 2, 1.5)


nLL_triangle2 <- function(x, a, b, c)
{
  n <- length(x)
  ind1 <- which(x < c)
  ind2 <- which(x >= c)
  n1 <- length(ind1)
  n2 <- length(ind2)
  if (n1 > 0 & n2 > 0)
  {
    return(-n*log(2) + n*log(b-a) + n1*log(c-a) + n2*log(b-c) - sum(log(x[ind1] - a)) - sum(log(b-x[ind2])))
  } else if (n1 == 0)
  {
    return(-n*log(2) + n*log(b-a) + n2*log(b-c) - sum(log(b-x[ind2])))
  } else if (n2 == 0)
  {
    return(-n*log(2) + n*log(b-a) + n1*log(c-a) - sum(log(x[ind1] - a)))
  }
}

temp <- triangle::rtriangle(100, 1, 3, 2)
nLL_triangle(temp, 0.5, 3.5, 2.2)
nLL_triangle2(temp, 0.5, 3.5, 2.2)

nLL_triangle(temp, 1, 4, 1)
nLL_triangle2(temp, 1, 4, 1)

nLL_triangle(temp, 1, 4, 4)
nLL_triangle2(temp, 1, 4, 4)

nLL_triangle(c(1,2,3), 1, 2.5, 4)
nLL_triangle2(c(1,2,3), 1, 2.5, 4)

triangle_ml <- function(x, start = NA, lower = NA, upper = NA, optim = stats::optim)
{
  nLL <- function(a, b, c)
  {
    nLL_triangle(x, a, b, c)
  }

  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  rangex <- maxx - minx
  if (any(is.na(start)))
  {
    # start at method of moments estimates, but a little wider
    start <- triangle_mom(x, na.rm = TRUE) + c(-0.5*rangex, 0.5*rangex, 0)
  }
  if (any(is.na(lower)) | any(is.na(upper)))
  {
    lower <- c(minx - 2*rangex, maxx, minx)
    upper <- c(minx, maxx + 2*rangex, maxx)
  }
  mle1 <- stats4::mle(nLL, start = start,
                      lower = lower,
                      upper = upper,
                      nobs = length(x))
  return(mle1)
}

triangle_ml(triangle::rtriangle(100, 1, 5, 4))
triangle_ml(triangle::rtriangle(100, 0.001, 0.005, 0.004))

triangle::dtriangle(triangle::rtriangle(100, 0.001, 0.005, 0.004))

# maximum likelihood estimate
temp <- triangle_ml(triangle::rtriangle(100, 1, 5, 4))
coef(temp)
# AIC
AIC(triangle_ml(triangle::rtriangle(100, 1, 5, 4)))
# BIC
BIC(triangle_ml(triangle::rtriangle(100, 1, 5, 4)))


triangle_ml2 <- function(x, start = NA, lower = NA, upper = NA)
{
  # x <- triangle::rtriangle(100, 0.001, 0.005, 0.004)
  nLL <- function(p, x)
  {
    nLL_triangle2(x, p[1], p[2], p[3])
  }

  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  rangex <- maxx - minx
  if (any(is.na(start)))
  {
    # start at method of moments estimates, but a little wider
    start <- triangle_mom(x, na.rm = TRUE) + c(-0.5*rangex, 0.5*rangex, 0)
  }
  if (any(is.na(lower)) | any(is.na(upper)))
  {
    lower <- c(minx - 2*rangex, # lower end of a can be low
               maxx + .Machine$double.eps*maxx, # lower end of b can be just above max(x)
               minx)
    upper <- c(minx - .Machine$double.eps*minx, maxx + 2*rangex, maxx)
  }
  mle1 <- optim(par = start, fn = nLL, x = x, method = "L-BFGS-B", lower = lower, upper = upper, hessian = TRUE, control = list(trace = 6))
  return(mle1)
}

triangle_ml(triangle::rtriangle(100, 1, 5, 4))
triangle_ml2(triangle::rtriangle(100, 1, 5, 4))
triangle_ml(triangle::rtriangle(100, 0.001, 0.005, 0.004))
triangle_ml2(triangle::rtriangle(100, 0.001, 0.005, 0.004))

nLL_triangle2(x, min(x)-1, max(x)+1, 0.003)

x <- triangle::rtriangle(100, 1, 5, 4)
plot(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l")
lines(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l", col ="red")
rug(x)

x <- triangle::rtriangle(10, 1, 5, 1)
plot(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l")
lines(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l", col ="red")
rug(x)

x <- triangle::rtriangle(10, 1, 5, 1)
plot(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l")
points(x, sapply(x, function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)))
rug(x)


x <- triangle::rtriangle(1000, 1, 5, 1)
system.time(sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)))
system.time(sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)))

gradient_nLL_triangle <- function(x, a, b, c)
{
  n <- length(x)
  ind1 <- which(x < c)
  ind2 <- which(x >= c)
  n1 <- length(ind1)
  n2 <- length(ind2)
  if (n1 > 0 & n2 > 0 & c > a & c < b)
  {
    return(c(-n/(b-a) - n1/(c-a) + sum(1/(x[ind1] - a)),
             n/(b-a) + n2/(b-c) - sum(1/(b-x[ind2])),
             n1/(c-a) - n2/(b-c)))
  } else if (n1 == 0)
  {
    return(c(-n/(b-a),
             n/(b-a) + n2/(b-c) - sum(1/(b-x[ind2])),
             - n2/(b-c)))
  } else if (n2 == 0)
  {
    return(c(-n/(b-a) - n1/(c-a) + sum(1/(x[ind1] - a)),
             n/(b-a),
             n1/(c-a)))
  }
}

hessian_nLL_triangle <- function(x, a, b, c)
{
  n <- length(x)
  ind1 <- which(x < c)
  ind2 <- which(x >= c)
  n1 <- length(ind1)
  n2 <- length(ind2)
  dadb <- n / (b-a)^2
  if (n1 > 0 & n2 > 0 & c > a & c < b)
  {
    da2 <- -n/(b-a)^2 - n1/(c-a)^2 + sum(1/(x[ind1] - a)^2)
    dadc <- n1 / (c-a)^2
    db2 <- -n/(b-a)^2 - n2/(b-c)^2 + sum(1/(b - x[ind2])^2)
    dbdc <- n2 / (b-c)^2
    dc2 <- -n1 / (c-a)^2 - n2 / (b-c)^2
  } else if (n1 == 0)
  {
    da2 <- -n/(b-a)^2
    dadc <- 0
    db2 <- -n/(b-a)^2 - n2/(b-c)^2 + sum(1/(b - x[ind2])^2)
    dbdc <- n2 / (b-c)^2
    dc2 <- -n2 / (b-c)^2
  } else if (n2 == 0)
  {
    da2 <- -n/(b-a)^2 - n1/(c-a)^2 + sum(1/(x[ind1] - a)^2)
    dadc <- n1 / (c-a)^2
    db2 <- -n/(b-a)^2
    dbdc <- 0
    dc2 <- -n1 / (c-a)^2
  }
  return(matrix(c(da2, dadb, dadc, dadb, db2, dbdc, dadc, dbdc, dc2), nrow = 3, dimnames = list(c("a","b","c"), c("a","b","c"))))
}

triangle_ml3 <- function(x, start = NA, lower = NA, upper = NA)
{
  # x <- triangle::rtriangle(100, 0.001, 0.005, 0.004)
  nLL <- function(p, x)
  {
    nLL_triangle2(x, p[1], p[2], p[3])
  }

  g_nLL <- function(p, x)
  {
    gradient_nLL_triangle(x, p[1], p[2], p[3])
  }

  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  rangex <- maxx - minx
  if (any(is.na(start)))
  {
    # start at method of moments estimates, but a little wider
    start <- triangle_mom(x, na.rm = TRUE) + c(-0.5*rangex, 0.5*rangex, 0)
  }
  if (any(is.na(lower)) | any(is.na(upper)))
  {
    lower <- c(minx - 2*rangex, # lower end of a can be low
               maxx + .Machine$double.eps*maxx, # lower end of b can be just above max(x)
               minx)
    upper <- c(minx - .Machine$double.eps*minx, maxx + 2*rangex, maxx)
  }
  mle1 <- optim(par = start, fn = nLL, gr = g_nLL, x = x, method = "L-BFGS-B", lower = lower, upper = upper, hessian = TRUE)
  print(mle1$hessian)
  print(hessian_nLL_triangle(x, mle1$par[1], mle1$par[2], mle1$par[3]))
  print(solve(mle1$hessian))
  print(solve(hessian_nLL_triangle(x, mle1$par[1], mle1$par[2], mle1$par[3])))
  return(mle1)
}

temp <- triangle::rtriangle(100, 1, 5, 4)
triangle_ml(temp)
triangle_ml2(temp)
out <- triangle_ml3(temp)
solve(out$hessian)

nLL_triangle2(temp, out$par[1], out$par[2], out$par[3])
(nLL_triangle2(temp, out$par[1], out$par[2], out$par[3]+0.01) - nLL_triangle2(temp, out$par[1], out$par[2], out$par[3]-0.01)) / 0.02
(nLL_triangle2(temp, out$par[1]+0.01, out$par[2], out$par[3]) - nLL_triangle2(temp, out$par[1]-0.01, out$par[2], out$par[3])) / 0.02
(nLL_triangle2(temp, out$par[1], out$par[2]+0.01, out$par[3]) - nLL_triangle2(temp, out$par[1], out$par[2]-0.01, out$par[3])) / 0.02
gradient_nLL_triangle(temp, out$par[1], out$par[2], out$par[3])

nLL_triangle2(temp, out$par[1], out$par[2], out$par[3])
(nLL_triangle2(temp, out$par[1]+.Machine$double.eps*2* out$par[1], out$par[2], out$par[3]) - nLL_triangle2(temp, out$par[1]-.Machine$double.eps*2* out$par[1], out$par[2], out$par[3])) / (.Machine$double.eps*4* out$par[1])
(nLL_triangle2(temp, out$par[1], out$par[2]+.Machine$double.eps*2* out$par[2], out$par[3]) - nLL_triangle2(temp, out$par[1], out$par[2]-.Machine$double.eps*2* out$par[2], out$par[3])) / (.Machine$double.eps*4* out$par[2])
(nLL_triangle2(temp, out$par[1], out$par[2], out$par[3]+.Machine$double.eps*2* out$par[3]) - nLL_triangle2(temp, out$par[1], out$par[2], out$par[3]-.Machine$double.eps*2* out$par[3])) / (.Machine$double.eps*4* out$par[3])
gradient_nLL_triangle(temp, out$par[1], out$par[2], out$par[3])


x <- triangle::rtriangle(10, 1, 5, 4)
plot(seq(min(x)+0.01, max(x)-0.01, length = 999), sapply(seq(min(x)+0.01, max(x)-0.01, length = 999), function(c) sum(log(triangle::dtriangle(x, min(x) - 0.01, max(x) + 0.01, c)))), type = "l")
rug(x)

plot(seq(min(x)+0.01, max(x)-0.01, length = 999), sapply(seq(min(x)+0.01, max(x)-0.01, length = 999), function(c) gradient_nLL_triangle(x, min(x) - 0.01, max(x) + 0.01, c)[3]), type = "l")
rug(x)

plot(seq(min(x)+0.01, max(x)-0.01, length = 999), sapply(seq(min(x)+0.01, max(x)-0.01, length = 999), function(c) hessian_nLL_triangle(x, min(x) - 0.01, max(x) + 0.01, c)[3,3]), type = "l")
rug(x)


temp <- triangle::rtriangle(100, 0.001, 0.005, 0.004)
triangle_ml(temp)
triangle_ml2(temp)
out <- triangle_ml3(temp)
solve(out$hessian)

triangle_ml2(triangle::rtriangle(100, 1, 5, 4))
triangle_ml(triangle::rtriangle(100, 0.001, 0.005, 0.004))
triangle_ml2(triangle::rtriangle(100, 0.001, 0.005, 0.004))

nLL_triangle2(x, min(x)-1, max(x)+1, 0.003)

x <- triangle::rtriangle(100, 1, 5, 4)
plot(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l")
lines(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l", col ="red")
rug(x)

x <- triangle::rtriangle(10, 1, 5, 1)
plot(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l")
lines(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l", col ="red")
rug(x)

x <- triangle::rtriangle(10, 1, 5, 1)
plot(seq(min(x), max(x), length = 999), sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)), type = "l")
points(x, sapply(x, function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)))
rug(x)


x <- triangle::rtriangle(1000, 1, 5, 1)
system.time(sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)))
system.time(sapply(seq(min(x), max(x), length = 999), function(c) nLL_triangle2(x, min(x) - .Machine$double.eps*min(x), max(x) + .Machine$double.eps*max(x), c)))


# traingle glm

triangle <- list(
  family = "triangle",
  link = "identity",
  linkfun = function(mu) {mu},
  linkinv = function(eta) {eta},
  variance = function(mu) {rep.int(1, length(mu))},
  dev.resids = function(y, mu, wt) {},
)

List of 12
$ family    : chr "binomial"
$ link      : chr "logit"
$ linkfun   :function (mu)
  $ linkinv   :function (eta)
    $ variance  :function (mu)
      $ dev.resids:function (y, mu, wt)
        $ aic       :function (y, n, mu, wt, dev)
          $ mu.eta    :function (eta)
            $ initialize: language {     if (NCOL(y) == 1) { ...
              $ validmu   :function (mu)
                $ valideta  :function (eta)
                  $ simulate  :function (object, nsim)
                    - attr(*, "class")= chr "family"


