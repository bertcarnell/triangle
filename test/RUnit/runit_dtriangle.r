###############################################################################
#
# Program Name:  testdtriangle.R
# Purpose:       To provide test functions for dtriangle.R
# Author:        Rob Carnell
# Date:          June 2006
#
# Required Functions: dtriangle.R
# Required Packages:  RUnit
# R version:          2.3.0 (>=2.0.0)
#
################################################################################

#source("c:/program files/R/triangle/R/dtriangle.R")
require(RUnit)

test.dtriangle <- function(){
  checkEqualsNumeric(dtriangle(1, 1, 3, 2), 0)
  checkEqualsNumeric(dtriangle(3, 0, 3, 2), 0)
  checkEqualsNumeric(dtriangle(-1, -1, 3, 2), 0)
  checkEqualsNumeric(dtriangle(.5, 0, 1, .5), 2)
  suppressWarnings(checkEquals(dtriangle(0, 0, 0, 0), NaN))
  suppressWarnings(checkEquals(dtriangle(.5, 0, 1, 2), NaN))
  suppressWarnings(checkEquals(dtriangle(.5, 1, 2, .5), NaN))
  checkEqualsNumeric(dtriangle(c(1, 3, -1, .5), c(1, 0, -1, 0), c(3, 3, 3, 1),
                               c(2, 2, 2, .5)), c(0, 0, 0, 2))
  suppressWarnings(checkEquals(dtriangle(c(.5, .6), c(0, 0), c(1, 1),
                                         c(.5, 1.5)), c(2, NaN)))
  # regression test
  checkTrue(all(!is.nan(dtriangle(0:10, a=2, b=5, c=2))))
  suppressWarnings(checkTrue(is.nan(dtriangle(.5, 1, 1, 1))))
  
  # more case testing
  checkEqualsNumeric(dtriangle(0, 1, 3, 2), 0)
  checkEqualsNumeric(dtriangle(1, 1, 3, 2), 0)
  checkEqualsNumeric(dtriangle(1.5, 1, 3, 2), 0.5)
  checkEqualsNumeric(dtriangle(2, 1, 3, 2), 1)
  checkEqualsNumeric(dtriangle(2.5, 1, 3, 2), 0.5)
  checkEqualsNumeric(dtriangle(3, 1, 3, 2), 0)
  checkEqualsNumeric(dtriangle(4, 1, 3, 2), 0)
  
  checkEqualsNumeric(dtriangle(0, 1, 3, 1), 0)
  checkEqualsNumeric(dtriangle(1, 1, 3, 1), 1)
  checkEqualsNumeric(dtriangle(2, 1, 3, 1), 0.5)
  checkEqualsNumeric(dtriangle(3, 1, 3, 1), 0)
  checkEqualsNumeric(dtriangle(4, 1, 3, 1), 0)
  
  checkEqualsNumeric(dtriangle(0, 1, 3, 3), 0)
  checkEqualsNumeric(dtriangle(1, 1, 3, 3), 0)
  checkEqualsNumeric(dtriangle(2, 1, 3, 3), 0.5)
  checkEqualsNumeric(dtriangle(3, 1, 3, 3), 1)
  checkEqualsNumeric(dtriangle(4, 1, 3, 3), 0)
}
