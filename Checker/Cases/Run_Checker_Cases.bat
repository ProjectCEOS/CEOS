@echo off

for /f %%a in ('dir * /a:d /b') do (

echo %%a

cd "%%a" 
rem O comando rem é para comentário. "%%a" pega o nome da pasta. echo escreve na tela
pause

call Run_CEOS.bat

call Run_Checker.bat

cd..

)

pause