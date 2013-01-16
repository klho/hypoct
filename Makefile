LIB       = hypoct
FC        = gfortran
FFLAGS    = -O3 -fPIC -fbounds-check
CFLAGS    = gcc
F2PY      = f2py
F2PYFLAGS = --fcompiler=gnu95

all: fortran python

fortran:
	$(FC) $(FFLAGS) -c $(LIB).f90

python: fortran
	$(F2PY) $(LIB).o -m $(LIB)_python -c $(LIB)_python.f90 $(F2PYFLAGS)

fortran_driver: fortran
	$(FC) $(FFLAGS) -c $(LIB)_driver.f90
	$(FC) -o $(LIB)_driver $(LIB)_driver.o hypoct.o
	./$(LIB)_driver

clean: clean_fortran clean_fortran_driver clean_python

clean_fortran:
	rm -f $(LIB).o $(LIB).mod $(LIB).pyc

clean_fortran_driver:
	rm -f $(LIB)_driver $(LIB)_driver.o

clean_python:
	rm -f $(LIB)_python.so
