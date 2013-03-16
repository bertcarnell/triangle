###############################################################################
#
# Program Name:  runit_ltriangle.R
# Purpose:       To provide test functions for ltriangle.R
# Author:        Rob Carnell
# Date:          June 2006
#
# Required Functions: rtriangle.R
# Required Packages:  RUnit
# R version:          2.3.0 (>=2.0.0)
#
################################################################################

require(RUnit)

test.rltriangle <- function()
{
  checkTrue(max(range(rltriangle(100))) <= 100)
  checkTrue(min(range(rltriangle(100))) >= 1)
  checkTrue(length(rltriangle(20.3)) == 20)

  checkException(rltriangle(-5), silent=TRUE)
  checkEquals(rltriangle(2, NaN, 3, 1), c(NaN, NaN))
  checkEquals(rltriangle(1, 3, NA, 4), NaN)
  checkException(rltriangle(NA), silent=TRUE)

  test <- rltriangle(1, 2, 5, 2+10^-6)
  checkTrue(test > 2 & test < 5)
  test <- rltriangle(1, 2, 5, 2+10^-9)
  checkTrue(test > 2 & test < 5)
  test <- rltriangle(1, 2, 5, 2+10^-12)
  checkTrue(test > 2 & test < 5)
  checkTrue(!is.nan(rltriangle(1, 2, 5, 2+10^-18)))

  checkTrue(!all(1 == rltriangle(100, 1, 100, 1)))
  checkTrue(!any(is.nan(rltriangle(100, 10, 100, 10))))
  checkTrue(all(!is.finite(rltriangle(10, -1, 0, -1))))
  checkTrue(all(is.na(rltriangle(10, -5, -2, -2))))
  checkTrue(all(1 == rltriangle(10,1,1,1)))
  checkTrue(all(is.na(rltriangle(10,-1,-1,-1))))
}

test.dltriangle <- function()
{
  checkEqualsNumeric(dltriangle(1, 1, 3, 2), 0)
  checkEqualsNumeric(dltriangle(3, 1, 3, 2), 0)
  checkEqualsNumeric(dltriangle(10^0.5, 1, 10, 10^0.5), 2)
  suppressWarnings(checkEquals(dltriangle(0, 0, 0, 0), NaN))
  suppressWarnings(checkEquals(dltriangle(.5, 0, 1, 2), NaN))
  suppressWarnings(checkEquals(dltriangle(.5, 1, 2, .5), NaN))
  checkEqualsNumeric(dltriangle(c(1, 3, 10^.5), c(1, 1, 1), c(3, 3, 10),
                               c(2, 2, 10^.5)), c(0, 0, 2))
}


test.ptriangle <- function()
{
  checkEqualsNumeric(pltriangle(1, 1, 3, 2), 0)
  checkEqualsNumeric(pltriangle(3, 1, 3, 2), 1)
  checkEqualsNumeric(pltriangle(10^.5, 1, 10, 10^.5), .5)
  suppressWarnings(checkEqualsNumeric(pltriangle(0, 0, 0, 0), NaN))
  suppressWarnings(checkEquals(pltriangle(.5, 0, 1, 2), NaN))
  suppressWarnings(checkEquals(pltriangle(.5, 1, 2, .5), NaN))
  checkEqualsNumeric(pltriangle(c(1, 3, 10^.5), c(1, 1, 1), c(3, 3, 10),
                               c(2, 2, 10^.5)), c(0, 1, .5))
}


test.qltriangle <- function()
{
  checkEqualsNumeric(qltriangle(0, 1, 3, 2), 1)
  checkEqualsNumeric(qltriangle(0, 2, 3, 2), 2)
  checkEqualsNumeric(qltriangle(1, 1, 3, 2), 3)
  checkEqualsNumeric(qltriangle(.5, 1, 10, 10^.5), 10^.5)
  checkEqualsNumeric(qltriangle(0, 1, 1, 1), 1)
  suppressWarnings(checkEquals(qltriangle(-1, 0, 1, .5), NaN))
  suppressWarnings(checkEquals(qltriangle(2, 0, 1, .5), NaN))
  suppressWarnings(checkEquals(qltriangle(.5, 0, 1, 2), NaN))
  suppressWarnings(checkEquals(qltriangle(.5, 1, 2, .5), NaN))
  checkEquals(qltriangle(.5, NA, 2, .5), NA)
  checkEqualsNumeric(qltriangle(c(0, 0, 1, .5), c(1, 2, 1, 1),
                               c(3, 3, 3, 10), c(2, 2, 2, 10^.5)),
                     c(1, 2, 3, 10^.5))
  try(checkException(qltriangle(c(0, 0, 0, 0, 0), c(0, 0), 1, .5)), silent=TRUE)
  checkEquals(qltriangle(.5, NaN, 2, 1), NaN)
  checkEquals(qltriangle(.5, 0, Inf, 1), NaN)
  checkEquals(qltriangle(.5, 0, Inf, Inf), NaN)

  checkEqualsNumeric(qltriangle(pltriangle(15, 1, 100, 10), 1, 100, 10), 15)
  checkEqualsNumeric(qltriangle(pltriangle(3, 2, 5, 5), 2, 5, 5), 3)
  checkEqualsNumeric(qltriangle(pltriangle(5, 2, 5, 5), 2, 5, 5), 5)
}
