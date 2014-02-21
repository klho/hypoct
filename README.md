hypoct
======

A hyperoctree is a geometrical tree data structure where each node is recursively subdivided into orthants. It is a multidimensional generalization of the binary tree in 1D, the quadtree in 2D, and the octree in 3D.

hypoct contains routines for constructing and manipulating point hyperoctrees. Its primary purpose is to support fast tree-based algorithms such as the fast multipole method. In this context, it provides an efficient hierarchical indexing scheme as well as encodes all near- and far-field information via neighbor and interaction lists.

Special features include:

- Compatibility with general elements by associating with each point a size. The effect of working with elements is that point distributions are no longer strictly contained within each node but can extend slightly beyond it. Elements are assigned to nodes that are at least four times their size and have a modified "two over" neighbor definition in order to fully capture the near field.

- Optimizations for non-uniform and high-dimensional data such as adaptive subdivision and pruning of empty leaves. In particular, short dimensions are not bisected in order to keep nodes as hypercubic as possible.

- Support for periodic domains.

- Support for "sparse elements" suitable for finite element computations. Sparse elements interact only by overlap and are assigned to nodes that are at least twice their size. The usual "one over" neighbor definition then suffices to capture all external interactions.

hypoct is written in Fortran with wrappers in C and Python. It is freely available under the GNU GPLv3; for alternate licenses, please contact the author.