###############################################################################
#
# Program Name:  testptriangle.R
# Purpose:       To provide test functions for ptriangle.R
# Author:        Rob Carnell
# Date:          June 2006
#
# Required Functions: ptriangle.R
# Required Packages:  RUnit
# R version:          2.3.0 (>=2.0.0)
#
################################################################################

#source("c:/program files/R/triangle/R/ptriangle.R")
require(RUnit)

test.ptriangle <- function(){
  checkEqualsNumeric(ptriangle(1, 1, 3, 2), 0)
  checkEqualsNumeric(ptriangle(3, 0, 3, 2), 1)
  checkEqualsNumeric(ptriangle(-1, -1, 3, 2), 0)
  checkEqualsNumeric(ptriangle(.5, 0, 1, .5), .5)
  suppressWarnings(checkEqualsNumeric(ptriangle(0, 0, 0, 0), NaN))
  suppressWarnings(checkEquals(ptriangle(.5, 0, 1, 2), NaN))
  suppressWarnings(checkEquals(ptriangle(.5, 1, 2, .5), NaN))
  checkEqualsNumeric(ptriangle(c(1, 3, -1, .5), c(1, 0, -1, 0), c(3, 3, 3, 1),
                               c(2, 2, 2, .5)), c(0, 1, 0, .5))
  suppressWarnings(checkEqualsNumeric(ptriangle(c(.5, .6), c(0, 0), c(1, 1),
                                      c(.5, 1.5)), c(.5, NaN)))
}
