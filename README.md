hypoct
======

A hyperoctree is a geometrical tree data structure where each node is
recursively subdivided into orthants. It is a multidimensional generalization of
the binary tree in 1D, the quadtree in 2D, and the octree in 3D.

HYPOCT contains procedures for constructing and manipulating point hyperoctrees.
Its primary purpose is to support fast tree-based algorithms such as the fast
multipole method. As such, it has capabilities for generating neighbor and
interaction lists, among others.

Special features include:

- Support for general elements (e.g., triangles) by associating with each point
  (e.g., centroid) a size (e.g., diameter). Larger elements are assigned to
  larger nodes so as to enforce the well-separated condition for non-self
  non-neighbors.

- Compatibility with point-point, point-element (collocation/qualocation), and
  element-element (Galerkin) interactions.

- Optimizations for non-uniform and high-dimensional data such as adaptive
  subdivision (at the scope of both nodes and levels) and pruning of empty
  nodes.

- Support for periodicity.

HYPOCT is written in Fortran with wrappers in C and Python. It is freely
available under the GNU GPLv3; for alternate licenses, please contact the author.