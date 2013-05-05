Introduction
============

A hyperoctree is a geometrical tree data structure where each node is recursively subdivided into orthants. It is a multidimensional generalization of the binary tree in 1D, the quadtree in 2D, and the octree in 3D.

hypoct contains routines for constructing and manipulating point hyperoctrees. Its primary purpose is to support fast tree-based algorithms such as the fast multipole method (FMM). In this context, it provides an efficient hierarchical indexing scheme as well as encodes all near- and far-field information via neighbor and interaction lists.

Special features include:

- Support for general elements (e.g., triangles) by associating with each point (e.g., centroid) a size (e.g., diameter). Larger elements are assigned to larger nodes so as to enforce the well-separated condition for non-self non-neighbors.

- Compatibility with point-point, point-element (collocation/qualocation), and element-element (Galerkin) interactions.

- Optimizations for non-uniform and high-dimensional data such as adaptive subdivision (of both nodes and levels) and pruning of empty leaves.

- Support for periodic domains.

hypoct is written in Fortran with wrappers in C and Python. This documentation mainly covers the Python interface, the intended method for most users.

Motivation and design
---------------------

To explain the motivation and design choices behind this library, it is perhaps easiest to begin by describing how a typical tree algorithm is implemented. For this, imagine a set of points distributed over some spatial region in :math:`d` dimensions. Most codes proceed by first enclosing all of the points within a sufficiently large hypercube called the root node. If the root contains more than a prescribed maximum number of points, then it is split into :math:`2^{d}` children of equal size via bisection in each dimension, and its points distributed accordingly between them. Each of these children are in turn subdivided if they contain too many points, and the process repeated for each new node added. The leaves of the tree are those nodes that do not have any children; they obviously satisfy the prescribed maximum occupancy condition.

General elements
................

In the fast algorithm context, a key piece of information that one would like to query from the tree is which points lie in the near and far fields of a given group of points as contained within a given node. A point is in the far field of a node if it is separated from that node by at least the node's size (also called well-separated). The near field is just the complement of the far field and can sometimes afford a more convenient description.

For points, which are zero-dimensional objects, the near field of a node is composed of precisely its neighbors, i.e., those nodes at the same level of the tree which immediately adjoin it. However, we are often interested in not just points but general elements, for example, triangles to describe a surface in 3D or cubes to describe a volume. These are characterized by a nonzero size (e.g., the diameter of the triangle or the extent of the cube), which can cause the above algorithm to fail its geometric responsibilities by not necessarily imposing that non-neighbors be well-separated. To see how, imagine many large elements packed closely together so that they are all in each others' near fields. If we sort only by, say, their centroids without regard for their size, then we can easily push the subdivision process well beyond what is appropriate, generating nontrivial far-field relationships between centroids that do not exist between the elements to which they belong.

One way to fix this problem is to adopt a more complex near-field definition than to simply identify a node's neighbors. The neighbor approach, however, is attractive---it is very natural and allows fast hierarchical computation---so we choose instead to modify the subdivision process. The key idea is that large elements must be assigned to nodes that are large enough to hold them, such that the neighbor definition of the near field remains valid. This means that not all elements are pushed down to the next level upon subdivision; large elements are held back to ensure correctness.

.. note::
   This also means that not all nodes need obey the maximum occupancy condition, though they will do so "as much as possible" given the size restriction. In particular, if a leaf contains only elements whose natural level is deeper than the leaf's level, then the leaf will satisfy the occupancy condition.

As an implementation matter, we stick to the point formulation for tree construction, but we associate with each point now a size corresponding to the size of the element which it represents. It is helpful to think of this size as the diameter of the element, and the point as its centroid (but it can be any point contained within). Then it is easy to verify that the following criteria are sufficient:

- For point-element interactions (such as those inherent in collocation or qualocation methods, where either an entire element interacts with a point or vice versa), assign elements to nodes that are at least twice its size.

- For element-element interactions (such as those inherent in Galerkin methods), assign elements to nodes that are at least thrice its size.

The algorithm is otherwise the same as before and the near field of a node remains specified by its neighbors.

Periodic domains
................

Problems involving periodic domains are encountered with some regularity and these, too, present troubles for standard tree codes. In this setting, the given data represent the unit cell, which is supplemented by additional parameters defining its extent. Building the tree itself is not problematic, but finding a neighbor structure compatible with the periodic geometry produces some difficulty. This is because the neighboring nodes across boundaries will in general not align with the node structure inside the unit cell. Thus, we must force the tree to conform to the problem geometry so that it is compatible with the periodicity.

We therefore allow the extent of the root node to be specified by the user (and otherwise calculated from the data if not supplied). Since this can lead to highly skewed aspect ratios, we also institute an adaptive subdivision criterion such that new nodes are "as cubic as possible". This is achieved by halving only those node dimensions that are at least :math:`1 / \sqrt{2}` times the maximum node dimension at each level, which ensures a bound of :math:`\sqrt{2}` on the aspect ratio for nodes at levels for which all dimensions are bisected.

Further adaptivity
..................

We have already seen some adaptivity in the splitting of nodes into children and the determination of node sizes, but for problems with non-uniform distributions or in high dimensions, further adaptivity is often desired. A somewhat annoying bottleneck in most codes is the creation of all :math:`2^{d}` children for a given node upon subdivision, which for large :math:`d` can become unwieldy. In this library, we therefore create only those nodes that are needed, allowing us to handle far larger dimensions than usual (if that ever becomes useful). This also has the effect of pruning empty leaves from the tree, which is a relatively standard feature.

Dynamic memory
..............

Finally, we wanted also to make use of some modern Fortran features to streamline the algorithm as compared to "classical" Fortran 77 programs. The main beneficiary is the use of dynamic memory. Older Fortran-style codes would typically run the tree algorithm twice: first to go through and count up the amount of memory required and then again to perform the actual memory writes once a properly sized block has been allocated. Here, we do essentially both at the same time by doubling the size of our working array each time that more memory is requested. All arrays are resized appropriately on output. Granted, this is a pretty minor point given all of the other languages out there with dynamic memory, but it is rather useful from a Fortran 77 standpoint.

Other features that we took advantage of include modules for better encapsulation of data and routines, and some shorthand notation for clarity and, perhaps, optimizability.

Algorithmic overview
--------------------

We have already discussed the tree construction process above. Briefly, to review, it consists of recursively subdividing nodes following a top-down sweep, alternately deciding which nodes to divide and then which points within those nodes to hold from further subdivision.

Finding neighbors similarly involves a top-down sweep. We first initialize the neighbors at the top two levels as a base case, then at each lower level search for the neighbors of each node among the children of its parent's neighbors. This hence nests the neighbor search hierarchically and results in good performance. Here, the neighbors of a given node are defined to be those nodes at the same level or higher that are nonempty and within one of the nodes' sizes of each other. A node is not considered its own neighbor.

For FMM codes, it is also useful to have access to the interaction list of a node, which consists of those nodes at the same level that are the children of its parent's neighbors but which are not themselves neighbors of the node. Interaction lists define a systematic multiscale tiling of space and provide an efficient organization of the main FMM computations. Here, we generate interaction lists as follows. First, we initialize the lists for the top three levels. Then for each node at a lower level, we simply apply the definition directly by searching among the children of its parent's neighbors.

For data distributions that are not too pathological, meaning here that the elements are not oversized and that the data do not consist of separate point clusters of vastly different scales (which, in principle, could be handled by constructing a tree on each cluster individually), the following complexity estimates hold, where :math:`N` is the number of points:

- The running time to build a tree scales as :math:`\mathcal{O} (N \log N)`, while its memory requirement is :math:`\mathcal{O} (N)`.

- Both the time and memory complexities for finding all neighbors are :math:`\mathcal{O} (N)`.

- Both the time and memory complexities for generating all interaction lists are :math:`\mathcal{O} (N)`.

Licensing and availability
--------------------------

hypoct is freely available under the `GNU GPL <http://www.gnu.org/licenses/gpl.html>`_ and can be downloaded at https://github.com/klho/hypoct. To request alternate licenses, please contact the author.