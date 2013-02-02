Tutorial
========

We now present a tutorial on using the Python interface to hypoct. From here on, we will therefore assume that hypoct has been properly installed along with its Python wrapper; if this is not the case, please go back to :doc:`install`. The choice to cover only the Python interface is merely for convenience. For help regarding the Fortran or C interfaces, please consult the corresponding source code and driver programs.

Overview
--------

The Python interface is located in the directory ``python``, which should contain directory ``hypoct``, organizing the main Python package, and ``hypoct_python.so``, the F2PY-ed Fortran library. The file ``hypoct_python.so`` contains all wrapped routines and is imported by :mod:`hypoct`, which creates a somewhat more convenient (but still pretty bare-bones) object-oriented interface around it. For details on the Python modules, please see the :doc:`api`; for details on data formats, please refer to the Fortran source code.

We will now step through the process of running a program calling hypoct from Python, following the Python driver program as a guide.

Initializing
------------

The first step is to import :mod:`hypoct` by issuing the command::

>>> import hypoct

at the Python prompt. This should work if you are in the ``python`` directory; otherwise, you may have to first type something like:

>>> import sys
>>> sys.path.append(/path/to/hypoct/python/)

in order to tell Python where to look.

Now let's generate some data. As an example, we consider points distributed uniformly on the unit circle::

>>> import numpy as np
>>> n = 100
>>> theta = np.linspace(0, 2*np.pi, n+1)[:n]
>>> x = np.array([np.cos(theta), np.sin(theta)])

Building the tree
-----------------

Building a tree can be as easy as typing::

>>> tree = hypoct.Tree(x)

Of course, this uses only the default options, e.g., sorting with a maximum occupany parameter of one, treating the data as zero-dimensional points, and no control over the extent of the root. To specify any of these, we can use keyword arguments as explained in :meth:`hypoct.Tree`. We also outline these briefly below.

Adaptivity setting
..................

To switch the build mode from adaptive to uniform subdivision (i.e., all leaves are divided if any one of them violates the occupany condition), enter::

>>> tree = hypoct.Tree(x, adap='u')

The default is ``adap='a'``.

Tree depth
..........

To control the level of subdivision, we can set the maximum leaf occupancy using the ``occ`` keyword. For example, to subdivide until all leaves contain no more than five points, we can type::

>>> tree = hypoct.Tree(x, occ=5)

The default is ``occ=1``.

It is also possible to set the maximum tree depth explicitly using the ``lvlmax`` keyword, e.g.::

>>> tree = hypoct.Tree(x, lvlmax=3)

The root is denoted as level zero. The default is ``lvlmax=-1``, which denotes no maximum depth. Both ``occ`` and ``lvlmax`` can be employed together, with ``lvlmax`` setting a hard limit on the tree.

Element interactions
....................

To treat the points as elements, each with a size, first create an array containing the size of each point, then build the tree using the ``siz`` keyword. Be sure also to specify whether the elements are meant to interact in a point-element (collocation/qualocation) or element-element (Galerkin) fashion as otherwise the size setting is ignored. For instance, to consider the points as elements each of size 0.1 interacting as in a collocation method, write::

>>> tree = hypoct.Tree(x, intr='c', siz=0.1)

where we have used the shorthand that if ``siz`` is a single number, then it is automatically expanded into an array of the appropriate size. Similarly, to specify a Galerkin-type interaction, write::

>>> tree =  hypoct.Tree(x, intr='g', siz=0.1)

The defaults are ``intr='p'``, corresponding to point-point interactions, and ``siz=0``.

Root extent
...........

The extent of the root node can be specified using the ``ext`` keyword, e.g.,

>>> tree = hypoct.Tree(x, ext=[10, 0])

This tells the code to set the length of the root along the first dimension to 10; its length along the second dimension is calculated from the data (the corresponding entry is nonpositive). This is often useful if there is some external parameter governing the problem geometry, for example, periodicity conditions. Like ``siz``, ``ext`` can also be given as a single number, in which case it is automatically expanded as appropriate. The default is ``ext=0``.

Remarks
.......

All options can be combined with each other. The output is stored as a :class:`hypoct.Tree` instance, which is a thin wrapper for the arrays output from Fortran. On our machine, running::

>>> tree = hypoct.Tree(x)
>>> tree.lvlx

gives::

  array([[  0,   1,   5,  17,  45,  97, 177, 193],
         [  6,   0,   3,   3,   3,   3,   3,   3]], dtype=int32)

which indicates that the tree has 6 levels (beyond the root) with 193 nodes in total. See the Fortran source code for details.

Generating auxiliary data
-------------------------

The base tree output is stored in a rather spartan manner; it contains only the bare minimum necessary to reconstruct the data for the entire tree. This is not always convenient and it is sometimes useful to have the data in a more easily accessible form. For instance, the base tree representation contains only parent and child identifier information that only really allows you to traverse a tree from the bottom up. To traverse a tree from the top down, we have to, in effect, generate child pointers, which we can do via::

>>> tree.generate_child_data()

We can also generate geometry information (center and extent) for each node by using::

>>> tree.generate_geometry_data()

These commands create the arrays ``tree.chldp``, and ``tree.l``, and ``tree.ctr``, respectively.

Finding neighbors
-----------------

To find the neighbors of each node, type::

>>> tree.find_neighbors()

which creates the neighbor pointer and index arrays ``tree.nborp`` and ``tree.nbori``, respectively. The method also accepts the keyword ``per`` indicating whether the root is periodic in a given dimension. For example, to impose that the root is periodic in the first but not the second dimension, set::

>>> tree.find_neighbors(per=[True, False])

It is worth emphasizing that the size of the unit cell cannot be directly controlled here; for this, use the ``ext`` keyword in :meth:`hypoct.Tree`. As with the ``siz`` and ``ext`` keywords for :meth:`hypoct.Tree`, we can also use shorthand by writing just, e.g.::

>>> tree.find_neighbors(per=True)

for double periodicity. The default is ``per=False``.

The method :meth:`hypoct.Tree.find_neighbors` requires that the child data from :meth:`hypoct.Tree.generate_child_data` have already been generated; if this is not the case, then this is done automatically.

Getting interaction lists
-------------------------

Recall that interaction lists are often utilized in fast multipole-type algorithms to systematically cover the far field. To get interaction lists for all nodes, type::

>>> tree.get_interaction_lists()

This command requires that the neighbor data from :meth:`hypoct.Tree.find_neighbors` have already been generated; if this is not the case, then this is done automatically using default settings. Outputs include the pointer and index arrays ``tree.ilstp`` and ``tree.ilsti``, respectively.

Putting it all together
-----------------------

A complete program for building a tree and generating all auxiliary data is given as follows::

  import hypoct, numpy as np

  # initialize points
  n = 100
  theta = np.linspace(0, 2*np.pi, n+1)[:n]
  x = np.array([np.cos(theta), np.sin(theta)])

  # build tree
  tree = hypoct.Tree(x, occ=5)
  tree.generate_child_data()
  tree.generate_geometry_data()
  tree.find_neighbors()
  tree.get_interaction_lists()

This is a slightly modified and abridged version of the driver program ``examples/hypoct_driver.py``.

Visualizing trees in 2D
-----------------------

Trees in 2D can be viewed graphically using the :class:`hypoct.tools.TreeVisualizer` class. To use the viewer, type::

>>> from hypoct.tools import TreeVisualizer
>>> view = TreeVisualizer(tree)
>>> view.draw_interactive()

This brings up an interactive session where each node in the tree is highlighted in turn, displaying its geometry, contained points, and neighbor and interaction list information, if available. Press ``Enter`` to step through the tree. All plot options can be controlled using :mod:`matplotlib`-style keywords.