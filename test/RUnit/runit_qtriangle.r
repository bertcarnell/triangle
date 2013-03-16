###############################################################################
#
# Program Name:  testqtriangle.R
# Purpose:       To provide test functions for qtriangle.R
# Author:        Rob Carnell
# Date:          June 2006
#
# Required Functions: qtriangle.R
# Required Packages:  RUnit
# R version:          2.3.0 (>=2.0.0)
#
################################################################################

#source("c:/program files/R/triangle/R/qtriangle.R")
require(RUnit)

test.qtriangle <- function(){
  checkEqualsNumeric(qtriangle(0, 1, 3, 2), 1)
  checkEqualsNumeric(qtriangle(0, 0, 3, 2), 0)
  checkEqualsNumeric(qtriangle(0, -1, 3, 2), -1)
  checkEqualsNumeric(qtriangle(1, 1, 3, 2), 3)
  checkEqualsNumeric(qtriangle(1, -3, -1, -2), -1)
  checkEqualsNumeric(qtriangle(.5, 0, 1, .5), .5)
  checkEqualsNumeric(qtriangle(0, 0, 0, 0), 0)
  suppressWarnings(checkEquals(qtriangle(-1, 0, 1, .5), NaN))
  suppressWarnings(checkEquals(qtriangle(2, 0, 1, .5), NaN))
  suppressWarnings(checkEquals(qtriangle(.5, 0, 1, 2), NaN))
  suppressWarnings(checkEquals(qtriangle(.5, 1, 2, .5), NaN))
  checkEquals(qtriangle(.5, NA, 2, .5), NA)
  checkEqualsNumeric(qtriangle(c(0, 0, 0, 1, 1, .5), c(1, 0, -1, 1, -3, 0),
                               c(3, 3, 3, 3, -1, 1), c(2, 2, 2, 2, -2, .5)),
                     c(1, 0, -1, 3, -1, .5))
  suppressWarnings(checkEquals(qtriangle(c(.5, -1, NA), c(0, 0, 0), c(1, 1, 1),
                                         c(.5, .5, .5)), c(.5, NaN, NA)))
  checkEqualsNumeric(qtriangle(c(0, 1, 0), 1, 2, 1.5), c(1, 2, 1))
  try(checkException(qtriangle(c(0, 0, 0, 0, 0), c(0, 0), 1, .5)), silent=TRUE)
  checkEquals(qtriangle(.5, NaN, 2, 1), NaN)
  checkEquals(qtriangle(.5, -Inf, 2, 1), NaN)
  checkEquals(qtriangle(.5, 0, Inf, 1), NaN)
  checkEquals(qtriangle(.5, -Inf, Inf, 1), NaN)
  checkEquals(qtriangle(.5, 0, Inf, Inf), NaN)

  # From a Bug Report, Michael.Scroggie@dse.vic.gov.au, Thursday 10/19/06
  checkTrue(!all(0 == qtriangle(runif(10), 0, 1, 0)))
  checkTrue(!all(5 == qtriangle(runif(10), 2, 5, 5)))
  checkTrue(all(1 == qtriangle(runif(10), 1, 1, 1)))
  checkTrue(all(-1 == qtriangle(runif(10), -1, -1, -1)))

  checkEqualsNumeric(qtriangle(ptriangle(.5, 0, 1, 0), 0, 1, 0), 0.5)
  checkEqualsNumeric(qtriangle(ptriangle(3, 2, 5, 5), 2, 5, 5), 3)
  checkEqualsNumeric(qtriangle(ptriangle(5, 2, 5, 5), 2, 5, 5), 5)
}
