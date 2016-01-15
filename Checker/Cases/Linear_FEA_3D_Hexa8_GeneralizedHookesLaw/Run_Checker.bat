@echo off

..\Checker.exe /File1 ceos_disp_x.probe /File2 ref_disp_x.dat /Tol 1.0d-3

..\Checker.exe /File1 ceos_disp_y.probe /File2 ref_disp_y.dat /Tol 1.0d-3

..\Checker.exe /File1 ceos_disp_z.probe /File2 ref_disp_z.dat /Tol 1.0d-3

..\Checker.exe /File1 ceos_stress_11.probe /File2 ref_stress_11.dat /Tol 1.0d-3

..\Checker.exe /File1 ceos_stress_33.probe /File2 ref_stress_33.dat /Tol 1.0d-3

..\Checker.exe /File1 ceos_stress_12.probe /File2 ref_stress_12.dat /Tol 1.0d-3


del FEMAnalysis.result
del *.probe
del NumberOfIterationsToConverge.dat