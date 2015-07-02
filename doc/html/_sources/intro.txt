Introduction
============

A hyperoctree is a geometrical tree data structure where each node is recursively subdivided into orthants. It is a multidimensional generalization of the binary tree in 1D, the quadtree in 2D, and the octree in 3D.

hypoct contains routines for constructing and manipulating point hyperoctrees. Its primary purpose is to support fast tree-based algorithms such as the fast multipole method (FMM). In this context, it provides an efficient hierarchical indexing scheme as well as encodes all near- and far-field information via neighbor and interaction lists.

Special features include:

- Compatibility with general elements by associating with each point a size. The effect of working with elements is that point distributions are no longer strictly contained within each node but can extend slightly beyond it. Elements are assigned to nodes that are at least four times their size and have a modified "two over" neighbor definition in order to fully capture the near field.

- Optimizations for non-uniform and high-dimensional data such as adaptive subdivision and pruning of empty leaves. In particular, short dimensions are not bisected in order to keep nodes as hypercubic as possible.

- Support for periodic domains.

- Support for "sparse elements" suitable for finite element computations. Sparse elements interact only by overlap and are assigned to nodes that are at least twice their size. The usual "one over" neighbor definition then suffices to capture all external interactions.

hypoct is written in Fortran with wrappers in C and Python. This documentation mainly covers the Python interface, the intended method for most users.

Motivation and design
---------------------

To explain the motivation and design choices behind this library, it is perhaps easiest to begin by describing how a typical tree algorithm is implemented. For this, imagine a set of points distributed over some spatial region in :math:`d` dimensions. Most codes proceed by first enclosing all of the points within a sufficiently large hypercube called the root node. If the root contains more than a prescribed maximum number of points, it is split into :math:`2^{d}` children of equal size via bisection in each dimension and its points distributed accordingly between them. Each of these children are in turn subdivided if they contain too many points, and the process repeated for each new node added. The leaves of the tree are those nodes that do not have any children; they obviously satisfy the prescribed maximum occupancy condition.

General elements
................

In the fast algorithm context, a key piece of information that one would like to query from the tree is which points lie in the near and far fields of a given cluster of points. A point is in the far field of a cluster if it is separated from that cluster by at least the cluster's size (also called *well-separated*). The near field is just the complement of the far field and can sometimes afford a more convenient description. A tree provides a natural clustering of points via the nodes to which they belong.

For true points, which are zero-dimensional objects, the near field of a node is composed of precisely its neighbors, i.e., those nodes of the tree which immediately adjoin it. However, we are often interested in not just points but general elements, for example, triangles to describe a surface in 3D or cubes to tessellate a volume. These are characterized by a nonzero size, e.g., the diameter of the triangle or the extent of the cube. We will stick to the point formulation for tree construction here, but we associate with each point now a size corresponding to the size of the element which it represents. It is helpful to think of this size as the diameter of the element and the point as its centroid, but it can be any point in or on the element. Then the problem as compared to the pure point setting is that elements can now extend beyond the boundaries of the node to which they belong. We can also regard elements as continuous point distributions, in which case the point distribution belonging to a given node is no longer contained strictly within it but rather within a slightly enlarged 'halo' region.

A consequence of this is that the usual "one over" neighbor definition is insufficient to impose that non-neighbors be well-separated. At minimum, the neighbors of a halo extension must include nodes that are at least "two over". We choose this as our modified neighbor definition for elements, which constrains that we assign elements to nodes that are at least four times their size. In particular, this means that not all elements are pushed down to the next level upon subdivision; large elements are held back to ensure correctness.

.. note::
   This also means that not all nodes need obey the maximum occupancy condition, though they will do so as much as possible given the size restriction. In particular, if a leaf contains only elements whose natural level is deeper than the leaf's level, then the leaf will satisfy the occupancy condition.

.. note::
   The simple neighbor definition above characterizes the case when two nodes are at the same level. When one node is larger than the other, the situation becomes slightly more complicated.

There is also an additional facility for elements that interact only sparsely, i.e., by overlap. This is suitable for, e.g., a finite element discretization of a partial differential equation. Sparse elements are assigned to nodes that are at least twice their size, for which the usual "one over" neighbor definition suffices to capture all external interactions. This substantially reduces the extent of the near field as compared with general 'dense' elements.

Periodic domains
................

Problems involving periodic domains are encountered with some regularity and these, too, present troubles for standard tree codes. In this setting, the given data represent the unit cell, which is supplemented by additional parameters defining its extent. Building the tree itself is not problematic, but finding a neighbor structure compatible with the periodic geometry produces some difficulty. This is because the neighboring nodes across boundaries will generally not align with the node structure inside the unit cell. Thus, we must force the tree to conform to the problem geometry so that it is compatible with the imposed periodicity.

We therefore allow the extent of the root node to be specified by the user (and otherwise calculated from the data if not supplied). Since this can lead to highly skewed aspect ratios, we also institute an adaptive subdivision criterion such that new nodes are as hypercubic as possible. This is achieved by halving only those node dimensions that are at least :math:`1 / \sqrt{2}` times the maximum node dimension at each level, which ensures a bound of :math:`\sqrt{2}` on the aspect ratio for nodes at levels for which all dimensions are bisected.

Further adaptivity
..................

We have already seen some adaptivity in the splitting of nodes above, but for problems with non-uniform distributions or in high dimensions, further adaptivity is often desired. A potential bottleneck in such codes is the creation of all :math:`2^{d}` children for a given node upon subdivision, which for large :math:`d` can become expensive. In this library, we therefore create only those nodes which are needed, allowing us to handle far larger dimensions than usual. This also has the effect of pruning empty leaves from the tree, which is a relatively standard feature.

Dynamic memory
..............

Finally, we wanted also to make use of some modern Fortran features to streamline the algorithm as compared to classical Fortran 77 programs. The main beneficiary is the use of dynamic memory. Older Fortran-style codes would typically run the tree algorithm twice: first to go through and count up the amount of memory required and then again to perform the actual memory writes once a properly sized block has been allocated. Here, we do essentially both at the same time by doubling the size of our working array each time that more memory is requested. All arrays are resized appropriately on output. Granted, this is a pretty minor point given all of the other languages out there with dynamic memory, but it is rather useful from a Fortran 77 standpoint.

Other features that we took advantage of include modules for better encapsulation of data and routines, and some shorthand notation for clarity and, perhaps, optimizability.

Algorithmic overview
--------------------

We have already discussed the tree construction process above. Briefly, to review, it consists of recursively subdividing nodes following a top-down sweep, alternately deciding which nodes to divide and then which points within those nodes to hold from further subdivision.

Finding neighbors similarly involves a top-down sweep. We first initialize the neighbors at the two coarsest levels as a base case, then at each finer level search for the neighbors of each node among the children of its parent's neighbors. This hence nests the neighbor search hierarchically and results in good performance.

For points, the neighbors of a given node consist of:

- All nodes at the same level immediately adjoining it ("one over").

- All non-empty nodes at a coarser level (parent or above) immediately adjoining it.

For elements, the neighbors of a given node consist of:

- All nodes at the same level separated by at most the node's size ("two over").

- All non-empty nodes at a coarser level (parent or above) whose halo extensions are separated its own extension by less than its extension's size.

Finally, for sparse elements, the neighbors consist of:

- All nodes at the same level immediately adjoining it ("one over").

- All non-empty nodes at a coarser level (parent or above) whose halo extensions overlap with its own extension.

In all cases, a node is not considered its own neighbor.

For FMM codes, it is also useful to have access to the interaction list of a node, which consists of:

- All nodes at the same level that are children of the neighbors of the node's parent but not neighbors of the node itself.

- All non-empty nodes at a coarser level (parent or above) that are neighbors of the node's parent but not neighbors of the node itself.

Interaction lists define a systematic multiscale tiling of space and provide an efficient organization of the main FMM computations. Here, we generate interaction lists as follows. First, we initialize the lists for the three coarsest levels. Then for each node at a finer level, we simply apply the definition directly by searching among the children of its parent's neighbors.

For data distributions that are not too pathological, meaning here that the elements are not oversized and that the data do not consist of separate point clusters of vastly different scales (which, in principle, could be handled by constructing a tree on each cluster individually), the following complexity estimates hold, where :math:`N` is the number of points:

- The running time to build a tree scales as :math:`\mathcal{O} (N \log N)`, while its memory requirement is :math:`\mathcal{O} (N)`.

- Both the time and memory complexities for finding all neighbors are :math:`\mathcal{O} (N)`.

- Both the time and memory complexities for generating all interaction lists are :math:`\mathcal{O} (N)`.

Licensing and availability
--------------------------

hypoct is freely available under the `GNU GPL <http://www.gnu.org/licenses/gpl.html>`_ and can be downloaded at https://github.com/klho/hypoct. To request alternate licenses, please contact the author.