@echo off

..\Checker.exe /File1 ceos_disp_x.probe /File2 marc_disp_x.dat /Tol 1.0d-3 

..\Checker.exe /File1 ceos_disp_y.probe /File2 marc_disp_y.dat /Tol 1.0d-3

..\Checker.exe /File1 ceos_disp_z.probe /File2 marc_disp_z.dat /Tol 1.0d-3 

..\Checker.exe /File1 ceos_disp_z.probe /File2 marc_disp_z.dat /Tol 1.0d-3 

..\Checker.exe /File1 NumberOfIterationsToConverge.dat /File2 NumOfIterRef.dat /Tol 1.0d-3

del FEMAnalysis.result
del *.probe
del NumberOfIterationsToConverge.dat