:: Set variables for inserting the repository number

::set mypath=.\lhs
::set exec=C:\Program Files\TortoiseSVN\bin\SubWCRev.exe

::"%exec%" "%mypath%" "%mypath%\ModifyInput\DESCRIPTION.input" "%mypath%\DESCRIPTION"
::"%exec%" "%mypath%" "%mypath%\ModifyInput\dirichlet-package.Rd.Input" "%mypath%\man\dirichlet-package.Rd"

:: Build R Package

set package=triangle

R CMD check %package%
R CMD build %package%
R CMD INSTALL %package%
R CMD INSTALL -build %package%

PAUSE
