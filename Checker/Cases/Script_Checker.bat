@echo off

echo ***************************************************************************** 
echo                               CEOS CHECKER
echo. 
echo ***************************************************************************** 
rem O comando rem é para comentário. "%%a" pega o nome da pasta. echo escreve na tela

echo.
echo Initializing CEOS check...

for /f %%a in ('dir * /a:d /b') do (

	echo.
	echo ============================================================================= 
	
	echo FOLDER CASE:  %%a
	cd "%%a"
	
	echo Running CEOS...
	call Run_CEOS.bat
	
	del Log_CHECKER.txt
	echo Running CHECKER...
	call Run_Checker.bat

	echo ============================================================================= 
	
	cd..
)


echo.
echo CEOS check is finished.

echo.
echo ***************************************************************************** 
echo.
echo Analysis details in Log_CEOS.txt and Log_CHECKER.txt into the folder cases. 
echo.
echo *****************************************************************************
echo.
