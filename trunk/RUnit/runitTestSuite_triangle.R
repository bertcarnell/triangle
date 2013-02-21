################################################################################
#
# Program Name:  runitTestSuite_triangle.R
# Purpose:       To provide test functions for teh triangle package
# Author:        Rob Carnell
# Date:          June 2006
#
# Required Packages:  RUnit
# R version:          2.3.0 (>=2.0.0)
#
################################################################################

require(RUnit)

## used as part of package
#require(triangle)
#defaultPath <- chartr("/", "//", paste(.path.package("triangle"), "/RUnit", sep=""))

################# used in development ##########################################
defaultPath <- file.path(getwd(), "Repositories", "triangle", "admin", "pkg", "triangle")
source(file.path(defaultPath, "R", "qtriangle.R"))
source(file.path(defaultPath, "R", "dtriangle.R"))
source(file.path(defaultPath, "R", "rtriangle.R"))
source(file.path(defaultPath, "R", "ptriangle.R"))
source(file.path(defaultPath, "R", "ltriangle.R"))

testSuite.triangle <- defineTestSuite("triangle",
  dirs=file.path(defaultPath, "..", "..", "trunk", "RUnit"),
  testFileRegexp="^runit_[rpqdl]triangle[.][rR]$")

testResult <- runTestSuite(testSuite.triangle)

################# used in development ##########################################

htmlFile <- file.path(defaultPath, "..", "..", "trunk", "RUnit", "Test_Results.html")

## warning expected about gcc compiler
suppressWarnings(printHTMLProtocol(testResult, fileName=htmlFile))

browseURL(htmlFile, browser=getOption("browser"))

