@ECHO off

ECHO #############################################
ECHO ABM MODEL INPUT CHECKER
ECHO.
ECHO Ashish Kulshrestha (kulshresthaa@pbworld.com)
ECHO Parsons Brinckerhoff
ECHO %Date%
ECHO #############################################

SET "curPath=%cd%"
SET "scriptPath=%curPath%\run.R"
set rPath="C:\Program Files\R\R-3.2.5\bin\x64\R.exe"

%rPath% CMD BATCH "%scriptPath%"