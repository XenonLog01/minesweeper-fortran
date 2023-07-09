@echo off

@rem For the source files, order matters!
set src_files=field main
set obj_files=

@rem Build the object files
for %%a in (%src_files%) do call :set_obj %%a

gfortran -o bin/app.exe %obj_files%
goto :end

:set_obj
  gfortran -c src/%1.f90 -o build/%1.o
  set obj_files=%obj_files% build/%1.o
:goto eof

:end
