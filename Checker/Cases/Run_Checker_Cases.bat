@echo off

for /f %%a in ('dir * /a:d /b') do (

echo %%a

cd "%%a" 

call Run_CEOS.bat

call Run_Checker.bat

cd..

)

pause