@echo off

rem O comando rem é para comentário. "%%a" pega o nome da pasta. echo escreve na tela
echo ***************************************************************************** 
echo                               CEOS CHECKER
echo. 
echo ***************************************************************************** 


echo.
echo Initializing CEOS check...

call Script_Checker.bat > Log_CEOS_CHECKER.txt

echo.
echo CEOS check is finished.

echo.
echo ***************************************************************************** 
echo.
echo Analysis details in Log_CEOS_CHECKER.txt
echo.
echo *****************************************************************************
echo.

pause