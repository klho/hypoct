LIB       = hypoct
FC        = gfortran
FFLAGS    = -fPIC -Wall -O3
CC        = gcc
CFLAGS    = -fPIC -Wall -O3
F2PY      = f2py3
F2PYFLAGS = --fcompiler=gnu95
PYTHON    = python

SRC      = src
BIN      = bin
C        = c
PYTHON   = python
DOC      = doc
EXAMPLES = examples

F2PY_EXT = $(shell python3-config --extension-suffix)
LIB_PY   = $(LIB)_python$(F2PY_EXT)

vpath %.f90 $(SRC)
vpath %.o   $(BIN)
vpath %.mod $(BIN)
vpath %.h   $(C)
vpath %.so  $(PYTHON)

.PHONY: all fortran c python doc fortran_driver c_driver python_driver clean clean_fortran clean_c clean_python clean_doc clean_driver rebuild help

all: fortran c python

$(LIB).o: $(LIB).f90
	$(FC) $(FFLAGS) -J$(BIN) -c $< -o $(BIN)/$@

$(LIB)_c.o: $(LIB)_c.f90 $(LIB).o
	$(FC) $(FFLAGS) -J$(BIN) -c $< -o $(BIN)/$@

$(LIB_PY): $(LIB)_python.f90 $(LIB).o
	ln -s $(BIN)/$(LIB).mod
	$(F2PY) -c -m $(LIB)_python $(F2PYFLAGS) $< $(BIN)/$(LIB).o
	rm -f $(LIB).mod
	mv $(LIB_PY) $(BIN)
	cd $(PYTHON) ; ln -fs ../$(BIN)/$(LIB_PY)

fortran: $(LIB).o

c: fortran $(LIB)_c.o

python: fortran $(LIB_PY)

doc: python
	cd $(DOC) ; make html ; make latexpdf

fortran_driver: fortran
	$(FC) -o $(EXAMPLES)/$(LIB)_driver $(FFLAGS) -I$(BIN) $(EXAMPLES)/$(LIB)_driver.f90 $(BIN)/$(LIB).o
	cd $(EXAMPLES) ; ./$(LIB)_driver

c_driver: c $(LIB).h
	$(CC) -o $(EXAMPLES)/$(LIB)_driver $(CFLAGS) -I$(BIN) -I$(C) $(EXAMPLES)/$(LIB)_driver.c $(BIN)/$(LIB).o $(BIN)/$(LIB)_c.o -lgfortran -lm
	cd $(EXAMPLES) ; ./$(LIB)_driver

python_driver: python
	cd $(EXAMPLES) ; python3 $(LIB)_driver.py

clean: clean_fortran clean_c clean_python clean_doc clean_driver

clean_fortran:
	rm -f $(BIN)/$(LIB).o $(BIN)/$(LIB).mod

clean_c:
	rm -f $(BIN)/$(LIB)_c.o $(BIN)/$(LIB)_c.mod

clean_python:
	cd $(BIN) ; rm -f $(LIB_PY)
	cd $(PYTHON) ; rm -f $(LIB_PY)
	cd $(PYTHON)/hypoct ; rm -rf __pycache__

clean_doc:
	cd $(DOC); make clean

clean_driver:
	cd $(EXAMPLES) ; rm -f $(LIB)_driver

rebuild: clean all

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  all            to make all code libraries (Fortran, C, Python)"
	@echo "  fortran        to make the Fortran library"
	@echo "  c              to make the C wrapper to the Fortran library"
	@echo "  python         to make the Python wrapper to the Fortran library"
	@echo "  doc            to make HTML and PDF documentation"
	@echo "  fortran_driver to make the Fortran driver program"
	@echo "  c_driver       to make the C driver program"
	@echo "  python_driver  to make the Python driver program"
	@echo "  clean          to remove all compiled objects"
	@echo "  clean_fortran  to remove all compiled Fortran objects"
	@echo "  clean_c        to remove all compiled C objects"
	@echo "  clean_python   to remove all compiled Python objects"
	@echo "  clean_doc      to remove all compiled documentation"
	@echo "  clean_driver   to remove all compiled driver executables"
	@echo "  rebuild        to clean and rebuild all libraries"
