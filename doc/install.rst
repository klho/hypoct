Installing
==========

This section describes how to compile and install hypoct on Unix-like systems. Primary prerequisites include `Git <http://git-scm.com/>`_, `GNU Make <http://www.gnu.org/software/make/>`_, and a Fortran compiler such as `GFortran <http://gcc.gnu.org/wiki/GFortran>`_. Secondary prerequisites, depending on which options are desired, include a C compiler such as `GCC <http://gcc.gnu.org/>`_ (for the C interface); `F2PY <http://www.scipy.org/F2py>`_, `Python <http://www.python.org/>`_, `NumPy <http://www.numpy.org/>`_, and `matplotlib <http://matplotlib.org/>`_ (for the Python interface); and `Sphinx <http://sphinx-doc.org/>`_ and `LaTeX <http://www.latex-project.org/>`_ (for the documentation).

hypoct has only been tested using the GFortran and GCC compilers; the use of all other compilers should be considered "at your own risk" (though they should really be fine).

Code repository
---------------

All source files for hypoct (including those for this documentation) are available at https://github.com/klho/hypoct. To download hypoct using Git, type the following command at the shell prompt::

$ git clone https://github.com/klho/hypoct /path/to/local/repository/

Compiling
---------

There are several targets available to compile, namely:

- the main Fortran library;

- the C wrapper for the Fortran library;

- the Python wrapper for the Fortran library;

- this documentation; and

- driver programs calling the library from each of the above languages.

To see all available targets, switch the working directory to the root of the local repository and type::

$ make

or::

$ make help

Hopefully the instructions are self-explanatory; for more explicit directions, please see below. Before beginning, view and edit the file ``Makefile`` to ensure that all options are properly set for your system. In particular, if you will not be using GFortran or GCC, be sure to set alternate compilers as appropriate.

To compile the main Fortran library, type::

$ make fortran

To compile the C wrapper, type::

$ make c

To compile the Python wrapper, type::

$ make python

To compile all three, type::

$ make all

All object files are placed in the directory ``bin/``.

To compile the documentation files, type::

$ make doc

Output HTML and PDF files are placed in the directory ``doc/``.

Driver programs
---------------

hypoct also contains driver programs in Fortran, C, and Python to demonstrate the use of the library and its various wrappers. To compile the drivers, type::

$ make fortran_driver

or::

$ make c_driver

or::

$ make python_driver

as appropriate. The above commands also automatically execute the corresponding programs. The driver programs are discussed in more detail in :doc:`tutorial`.