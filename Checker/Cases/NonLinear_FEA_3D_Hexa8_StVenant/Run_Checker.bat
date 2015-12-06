@echo off

..\Checker.exe /File1 ceos_disp_x.probe /File2 marc_disp_x.dat /Tol 1.0d-5

..\Checker.exe /File1 ceos_disp_y.probe /File2 marc_disp_y.dat /Tol 1.0d-5

..\Checker.exe /File1 ceos_disp_z.probe /File2 marc_disp_z.dat /Tol 1.0d-5


del FEMAnalysis.result