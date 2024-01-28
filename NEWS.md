# version 0.1 (2006-10-20)

- Changed the rtraingle and qtriangle functions in response to an excellent bug
report from Dr Michael Scroggie (Michael.Scroggie@dse.vic.gov.au).  The bug
involved the conditional logic in the inverse CDF so that when the "a" and "c"
parameters were equal, rtriangle would return a vector of "a".

# version 0.4

- Changed the license to GPL >= 2 according to a Kurt Hornik email

# version 0.5

- Changed the filenames to be portable

# version 0.6 (2009-03-25)

- added the log scale triangle function

# version 0.7

- Added the manual entry for the logarithmic triangle

# version 0.8

- Changed the arguments for dtriangle and dltriangle to be an "x" instead of a "q"
based on a suggestion from Randall Pruim.  Also, changed the default for the
triangle mode to be 1/2 of the base instead of 1/2 (or 10 for the log triangle)
based on Randall Pruim's suggestion as well.

# version 0.9

- Updates to fix errors in the CRAN build.

# version 0.10

- Update to the description file

# version 0.11

- Update to dtriangle to fix a bug when a == c and x == c

# version 0.12

- Update to move from R-forge to github

# version 1.0

- Included maximum likelihood and methods of moments estimates

# version 1.0.1

- Documentation fixes and website formatting
