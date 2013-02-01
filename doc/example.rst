Examples
========

We give some further examples on using hypoct in this section. Some of these can also be considered tests. Insert into the preamble of each code snippet the following::

  import hypoct, numpy as np
  from hypoct.tools import TreeVisualizer

Degenerate distributions
------------------------

A good test of robustness is to run hypoct on data that is degenerate in one or more dimensions::

  x = np.random.rand(2, 100)
  x[0,:] = 0
  tree = hypoct.Tree(x)

The following output shows that the test succeeds::

  >>> tree.lvlx

  array([[  0,   1,   3,   7,  15,  31,  61, 105, 159, 193, 215, 231, 239,
        245, 249],
         [ 13,   0,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
          2,   2]], dtype=int32)

Note the string of twos in ``tree.lvlx[1,:]``, which indicates that subdivision occurred only along the second dimension and not the first (which has zero extent).

High-dimensional data
---------------------

Another good test is on high-dimensional data. Here, we try :math:`d = 30`, for which :math:`2^{d} \sim 10^{9}`, near the maximum range of four-byte integer values::

  x = np.random.rand(30, 100)
  tree = hypoct.Tree(x)
  tree.find_neighbors()

The code executes successfully with outputs::

  >>> tree.lvlx

  array([[         0,          1,        101],
         [         1,          0, 1073741823]], dtype=int32)

  >>> tree.nborp

  array([   0,    0,   99,  198,  297,  396,  495,  594,  693,  792,  891,
          990, 1089, 1188, 1287, 1386, 1485, 1584, 1683, 1782, 1881, 1980,
         2079, 2178, 2277, 2376, 2475, 2574, 2673, 2772, 2871, 2970, 3069,
         3168, 3267, 3366, 3465, 3564, 3663, 3762, 3861, 3960, 4059, 4158,
         4257, 4356, 4455, 4554, 4653, 4752, 4851, 4950, 5049, 5148, 5247,
         5346, 5445, 5544, 5643, 5742, 5841, 5940, 6039, 6138, 6237, 6336,
         6435, 6534, 6633, 6732, 6831, 6930, 7029, 7128, 7227, 7326, 7425,
         7524, 7623, 7722, 7821, 7920, 8019, 8118, 8217, 8316, 8415, 8514,
         8613, 8712, 8811, 8910, 9009, 9108, 9207, 9306, 9405, 9504, 9603,
         9702, 9801, 9900], dtype=int32)

  >>> tree.nbori

  array([  3,   4,   5, ...,  98,  99, 100], dtype=int32)

For higher dimensions, hypoct must be modified and recompiled to use extended precision integers, e.g., ``integer*8`` in Fortran, ``long long`` in C, and ``int64`` in Python (NumPy).


Periodic data
-------------

To construct a triply periodic tree with unit cell extents of, say, 10, 2, and 2, respectively, in the first, second, and third dimensions, write, e.g.::

  x = np.random.rand(3, 100)
  tree = hypoct.Tree(x, ext=[10., 2., 2.])
  tree.find_neighbors(per=True)
  tree.get_interaction_lists()

The periodicity of the interaction lists inherits from that of the neighbor list.

Tree for Galerkin triangle code
-------------------------------

The following code demonstrates how to build a tree on triangles that is suitable for Galerkin computations::

  # format for `vert`: vertex `i` of triangle `j` has coordinates `vert[:,i,j]`
  n = vert.shape[2]

  # compute triangle centroids
  cent = vert.mean(axis=1)

  # compute triangle diameters
  diam = np.empty(n)
  for i in range(n):
    diam[i] = max(vert[:,:,i].max(axis=1) - vert[:,:,i].min(axis=1))

  # build tree
  tree = hypoct.Tree(cent, intr='g', siz=diam)

Changing plot styles for TreeVisualizer
----------------------------------------

Plot styles for :meth:`hypoct.tools.TreeVisualizer.draw_interactive` can be changed by specifying :mod:`matplotlib`-type keywords. For example, using::

  view = TreeVisualizer(tree)
  view.draw_interactive(node_alpha=0.2, point_c='g', nbor_color='y', ilst_color='r')

sets the transparency level for the current node patch to 0.2, the color for points contained within the current node to green, the color of neighboring node patches to yellow, and the color of node patches in the interaction list to red.