###############################################################################
#
# Program Name:  testrtriangle.R
# Purpose:       To provide test functions for rtriangle.R
# Author:        Rob Carnell
# Date:          June 2006
#
# Required Functions: rtriangle.R
# Required Packages:  RUnit
# R version:          2.3.0 (>=2.0.0)
#
################################################################################

#source("c:////program files//R//triangle//R//rtriangle.R")
require(RUnit)

test.rtriangle <- function(){
  #checkEqualsNumeric({set.seed(1976); rtriangle(1, 0, 1, .5)}, 0.1993473295,
  #                   tolerance=1E-7)
  #checkEqualsNumeric({set.seed(1972); rtriangle(2, -5, 5, 3)},
  #                   c(2.64973641, -0.09889617), tolerance=1E-7)
  checkTrue(max(range(rtriangle(100))) <= 1)
  checkTrue(min(range(rtriangle(100))) >= 0)
  checkTrue(length(rtriangle(20.3)) == 20)

  checkException(rtriangle(-5), silent=TRUE)
  checkEquals(rtriangle(2, NaN, 3, 1), c(NaN, NaN))
  checkEquals(rtriangle(1, 3, NA, 4), NaN)
  checkException(rtriangle(NA), silent=TRUE)

  # From a Bug Report, Michael.Scroggie@dse.vic.gov.au, Thursday 10/19/06
  checkTrue(!all(0 == rtriangle(1, 0, 1, 0)))
  checkTrue(!all(1 == rtriangle(1, 0, 1, 1)))
  checkTrue(!all(5 == rtriangle(1, 2, 5, 5)))
  checkTrue(!identical(NaN, rtriangle(1, 2, 5, 2)))
  
  test <- rtriangle(1, 2, 5, 2+10^-6)
  checkTrue(test > 2 & test < 5)
  test <- rtriangle(1, 2, 5, 2+10^-9)
  checkTrue(test > 2 & test < 5)
  test <- rtriangle(1, 2, 5, 2+10^-12)
  checkTrue(test > 2 & test < 5)
  checkTrue(!is.nan(rtriangle(1, 2, 5, 2+10^-18)))

  checkTrue(!all(0 == rtriangle(100, 0, 100, 0)))
  checkTrue(!any(is.nan(rtriangle(100, 10, 100, 10))))
  checkTrue(!all(-1 == rtriangle(10, -1, 0, -1)))
  checkTrue(!all(-2 == rtriangle(10, -5, -2, -2)))
  checkTrue(all(1 == rtriangle(10,1,1,1)))
  checkTrue(all(-1 == rtriangle(10,-1,-1,-1)))
}
