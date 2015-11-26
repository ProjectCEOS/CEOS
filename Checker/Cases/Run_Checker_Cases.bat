@echo off

for /f %%a in ('dir * /a:d /b') do (

echo %%a

cd "%%a" 

call Run_Case.bat

cd..

)

pause