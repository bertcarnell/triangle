:: Build R Package
set Rversion=R-3.2.3
set Rcommand="C:\Program Files\R\%Rversion%\bin\R.exe"
set package=triangle

if "%1" == "check" (
	cd ..\pkg
	%Rcommand% CMD check %package%
	cd ..\trunk
) else if "%1" == "build" (
	cd ..\pkg
	%Rcommand% CMD build %package%
	%Rcommand% CMD INSTALL --build *.tar.gz
	cd ..\trunk
) else if "%1" == "install" (
	cd ..\pkg
	%Rcommand% CMD INSTALL %package%
	cd ..\trunk
)

PAUSE
