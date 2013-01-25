LIB       = hypoct
FC        = gfortran
FFLAGS    = -fPIC -Wall -O3 -fbounds-check
CC        = gcc
CFLAGS    = -fPIC -Wall -O3 -fbounds-check
F2PY      = f2py
F2PYFLAGS = --fcompiler=gnu95 -DF2PY_REPORT_AT_EXIT -DF2PY_REPORT_ON_ARRAY_COPY
PYTHON    = python

SRC      = src
BIN      = bin
C        = c
PYTHON   = python
DOC      = doc
EXAMPLES = examples

vpath %.f90 $(SRC)
vpath %.o   $(BIN)
vpath %.mod $(BIN)
vpath %.h   $(C)
vpath %.so  $(PYTHON)

.PHONY: help all doc fortran c python fortran_driver c_driver python_driver clean clean_fortran clean_c clean_python clean_driver rebuild

help:
	@echo help

$(LIB).o: $(LIB).f90
	$(FC) $(FFLAGS) -J$(BIN) -c $< -o $(BIN)/$@

$(LIB)_c.o: $(LIB)_c.f90 $(LIB).o
	$(FC) $(FFLAGS) -J$(BIN) -c $< -o $(BIN)/$@

$(LIB)_python.so: $(LIB)_python.f90 $(LIB).o
	ln -s $(BIN)/$(LIB).mod
	$(F2PY) -c -m $(LIB)_python $(F2PYFLAGS) $< $(BIN)/$(LIB).o
	rm -f $(LIB).mod
	mv $(LIB)_python.so $(BIN)
	cd $(PYTHON) ; ln -fs ../$(BIN)/$(LIB)_python.so

all: fortran c python doc

fortran: $(LIB).o

c: fortran $(LIB)_c.o

python: fortran $(LIB)_python.so

doc:

fortran_driver: fortran
	$(FC) -o $(EXAMPLES)/$(LIB)_driver $(FFLAGS) -I$(BIN) $(EXAMPLES)/$(LIB)_driver.f90 $(BIN)/$(LIB).o
	cd $(EXAMPLES) ; ./$(LIB)_driver

c_driver: c $(LIB).h
	$(CC) -o $(EXAMPLES)/$(LIB)_driver $(CFLAGS) -I$(BIN) -I$(C) -lgfortran -lm $(EXAMPLES)/$(LIB)_driver.c $(BIN)/$(LIB).o $(BIN)/$(LIB)_c.o
	cd $(EXAMPLES) ; ./$(LIB)_driver

python_driver: python
	cd $(EXAMPLES) ; python $(LIB)_driver.py

clean: clean_fortran clean_c clean_python clean_driver

clean_fortran:
	rm -f $(BIN)/$(LIB).o $(BIN)/$(LIB).mod

clean_c:
	rm -f $(BIN)/$(LIB)_c.o $(BIN)/$(LIB)_c.mod

clean_python:
	cd $(BIN) ; rm -f $(LIB)_python.so
	cd $(PYTHON) ; rm -f $(LIB)_python.so $(LIB).pyc TreeVisualizer.pyc

clean_driver:
	cd $(EXAMPLES) ; rm -f $(LIB)_driver

rebuild: clean all