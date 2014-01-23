!*******************************************************************************
!   Copyright (C) 2013-2014 Kenneth L. Ho
!
!   This program is free software: you can redistribute it and/or modify it
!   under the terms of the GNU General Public License as published by the Free
!   Software Foundation, either version 3 of the License, or (at your option)
!   any later version.
!
!   This program is distributed in the hope that it will be useful, but WITHOUT
!   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
!   more details.
!
!   You should have received a copy of the GNU General Public License along with
!   this program.  If not, see <http://www.gnu.org/licenses/>.
!*******************************************************************************

!*******************************************************************************
    module hypoct
!*******************************************************************************
!   HYPOCT - hyperoctree construction and manipulation
!
!   A hyperoctree is a geometrical tree data structure where each node is
!   recursively subdivided into orthants. It is a multidimensional
!   generalization of the binary tree in 1D, the quadtree in 2D, and the octree
!   in 3D.
!
!   HYPOCT contains routines for constructing and manipulating point
!   hyperoctrees. Its primary purpose is to support fast tree-based algorithms
!   such as the fast multipole method. In this context, it provides an efficient
!   hierarchical indexing scheme as well as encodes all near- and far-field
!   information via neighbor and interaction lists.
!
!   Special features include:
!
!   - Compatibility with general elements by associating with each point a size.
!     Larger elements are assigned to larger nodes so as to enforce the
!     well-separated condition for non-self non-neighbors. The effect of working
!     with elements is that point distributions are now no longer strictly
!     contained within each node but can extend slightly beyond it.
!
!   - Optimizations for non-uniform and high-dimensional data such as adaptive
!     subdivision and pruning of empty leaves. In particular, short dimensions
!     are not bisected in order to keep nodes as hypercubic as possible.
!
!   - Support for periodic domains.
!
!   HYPOCT is written mainly in a Fortran 77 style for compatibility, with
!   select modern Fortran features for performance and accessibility.
!
!   ----------------------------------------------------------------------------
!   User-level routines
!   ----------------------------------------------------------------------------
!   hypoct_build  - build hyperoctree
!   hypoct_chld   - generate child data
!   hypoct_geom   - generate geometry data
!   hypoct_ilst   - get interaction lists
!   hypoct_nbor   - find neighbors
!   hypoct_search - search hyperoctree
!*******************************************************************************
     implicit none

    contains

!===============================================================================
!    User-level routines
!===============================================================================

!*******************************************************************************
     subroutine hypoct_build(adap, elem, d, n, x, siz, occ, lvlmax, ext, &
                             lvlx, rootx, xi, xp, nodex)
!*******************************************************************************
!    Build hyperoctree.
!
!    Arguments
!    =========
!
!    ADAP : CHARACTER, INTENT(IN)
!      Adaptivity setting:
!        ADAP = 'a': adaptive
!        ADAP = 'u': uniform
!      This specifies whether nodes are divided adaptively or to a uniformly
!      fine level.
!
!    ELEM : CHARACTER, INTENT(IN)
!      Element type:
!        ELEM = 'p': points
!        ELEM = 'e': elements
!        ELEM = 's': sparse elements
!      This specifies whether the input points represent true points or general
!      elements (with sizes) that can extend beyond node boundaries. Larger
!      elements are assigned to larger nodes. If ELEM = 'p', then SIZ is
!      ignored. The distinction between elements and sparse elements is that
!      elements are intended to interact densely with each other, whereas sparse
!      elements interact only with those which they overlap.
!
!    D : INTEGER, INTENT(IN)
!      Dimension of space. Requires D > 0.
!
!    N : INTEGER, INTENT(IN)
!      Number of points. Requires N > 0.
!
!    X : REAL*8, DIMENSION(D,N), INTENT(IN)
!      Point coordinates.
!
!    SIZ : REAL*8, DIMENSION(N), INTENT(IN)
!      Sizes associated with each point. Not accessed if ELEM = 'p'.
!
!    OCC : INTEGER, INTENT(IN)
!      Maximum leaf occupancy. Requires OCC > 0.
!
!    LVLMAX : INTEGER, INTENT(IN)
!      Maximum tree depth. The root is defined to have level zero. No maximum if
!      LVLMAX < 0.
!
!    EXT : REAL*8, DIMENSION(D), INTENT(IN)
!      Extent of root node. If EXT(I) <= 0, then the extent in dimension I is
!      calculated from the data. Its primary use is to force nodes to conform to
!      a specified geometry (see HYPOCT_NBOR).
!
!    LVLX : INTEGER, ALLOCATABLE, DIMENSION(:,:), INTENT(OUT)
!      Level data array. On exit, LVLX has dimension (2,0:NLVL+1), where
!      NLVL = LVLX(2,0) is the tree depth, and contains the following
!      information:
!        LVLX(1,:)  - level pointer array
!        LVLX(2,0)  - tree depth
!        LVLX(2,1:) - level subdivision indices
!      The nodes on level I have ordered indices LVLX(1,I)+1:LVLX(1,I+1). Each
!      level subdivison index is an integer bitstring denoting the dimensions
!      that have been bisected.
!
!    ROOTX : REAL*8, DIMENSION(2,D) INTENT(OUT)
!      Root data array, containing the following information:
!        ROOTX(1,:) - center of root node
!        ROOTX(2,:) - extent of root node
!
!    XI : INTEGER, DIMENSION(N), INTENT(OUT)
!      Sorted point indices.
!
!    XP : INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT)
!      Node pointer array. On exit, XP has dimension NNODE + 1, where
!      NNODE = LVLX(1,NLVL+1) is the total number of nodes. The points in node I
!      have indices XI(XP(I)+1:XP(I+1)).
!
!    NODEX : INTEGER, ALLOCATABLE, DIMENSION(:,:), INTENT(OUT)
!      Node data array. On exit, NODEX has dimension (2,NNODE), where NNODE is
!      the total number of nodes, and contains the following information:
!        NODEX(1,:) - node parents
!        NODEX(2,:) - node child IDs
!      Each node child ID is an integer bitstring denoting the dimensions for
!      which the node is on the positive side of its parent's center.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: adap, elem
      integer, intent(in) :: d, n, occ, lvlmax
      real*8, intent(in) :: x(d,n), siz(n), ext(d)
      integer, intent(out) :: xi(n)
      integer, allocatable, intent(out) :: lvlx(:,:), xp(:), nodex(:,:)
      real*8, intent(out) :: rootx(2,d)

!     local variables
      character(len=*), parameter :: srname = 'HYPOCT_BUILD'
      integer :: mlvl, nlvl, mnode, mleaf, nnode, nnode_, nleaf, lvldiv, i
      integer, allocatable :: leaf(:)
      real*8, parameter :: divrat = 1d0/sqrt(2d0)
      real*8 :: xrng(2), l(d), lrng(2)
      real*8, allocatable :: ctr(:,:)
      logical :: malloc
      logical, allocatable :: div(:)
!     ==========================================================================

!     handle input errors
      if ((adap /= 'a') .and. (adap /= 'u')) call hypoct_errprm(srname, 'ADAP')
      if ((elem /= 'p') .and. (elem /= 'e') .and. (elem /= 's')) then
        call hypoct_errprm(srname, 'ELEM')
      endif
      if (d <= 0) call hypoct_errprm(srname, 'D')
      if (n <= 0) call hypoct_errprm(srname, 'N')
      if (occ <= 0) call hypoct_errprm(srname, 'OCC')

!     initialize
      mlvl = max(lvlmax, 1)
      mnode = 1
      mleaf = 1
      allocate(lvlx(2,0:mlvl+1), xp(mnode+1), nodex(2,mnode), ctr(d,mleaf), &
               leaf(mleaf+1), div(mleaf))
      lvlx(1,0) = 0
      lvlx(1,1) = 1
      lvlx(2,1) = 0
      do i = 1, d
        xrng(1) = minval(x(i,:))
        if (ext(i) > 0) then
          xrng(2) = xrng(1) + ext(i)
        else
          xrng(2) = maxval(x(i,:))
        endif
        rootx(1,i) = 0.5d0*(xrng(1) + xrng(2))
        rootx(2,i) =        xrng(2) - xrng(1)
      enddo
      xp(1:2) = (/ 0, n /)
      nodex(:,1) = (/ 0, -1 /)
      xi = (/ (i, i = 1, n) /)
      leaf(1:2) = xp(1:2)
      ctr(:,1) = rootx(1,:)
      l = rootx(2,:)
      lrng = (/ minval(l), maxval(l) /)
      nlvl  = 0
      nnode = 1
      nleaf = 1

!     build tree
      do while (.true.)

!       check for termination
        if (nlvl == lvlmax) exit

!       determine nodes to subdivide
        div(1:nleaf) = .false.
        do i = 1, nleaf
          if (leaf(i+1) - leaf(i) > occ) then
            if (adap == 'a') then
              div(i) = .true.
            elseif (adap == 'u') then
              div(1:nleaf) = .true.
              exit
            endif
          endif
        enddo
        if (.not. any(div(1:nleaf))) exit

!       divide sides (if not too short)
        lvldiv = 0
        do i = 1, d
          if (l(i) > divrat*lrng(2)) then
            lvldiv = ibset(lvldiv, i-1)
            l(i) = 0.5d0*l(i)
          endif
        enddo
        lrng = (/ minval(l), maxval(l) /)

!       hold points at current level
        call hypoct_hold(elem, siz, lrng(1), nleaf, leaf, div, &
                         xi(xp(lvlx(1,nlvl)+1)+1), xp(lvlx(1,nlvl)+1))

!       stop if none left
        if (.not. any(div(1:nleaf))) exit

!       divide leaves
        nnode_ = nnode
        do i = 1, nleaf
          if (.not. div(i)) cycle
          call hypoct_subdiv(d, x, lvldiv, lvlx(1,nlvl)+i, ctr(1,i), &
                             leaf(i+1)-leaf(i), &
                             xi(xp(lvlx(1,nlvl+1)+1)+leaf(i)+1), &
                             mnode, nnode, xp, nodex)
        enddo

!       update leaf data
        nleaf = nnode - nnode_
        malloc = .false.
        do while (nleaf > mleaf)
          malloc = .true.
          mleaf = 2*mleaf
        enddo
        if (malloc) then
          call hypoct_malloc2d(ctr, d, mleaf, .true.)
          call hypoct_malloc1i(leaf, mleaf+1, .true.)
          call hypoct_malloc1l(div, mleaf, .false.)
        endif
        call hypoct_update(lvlx(1,nlvl), xp(lvlx(1,nlvl+1)+1), &
                           nodex(1,lvlx(1,nlvl+1)+1), d, l, lvldiv, &
                           nleaf, leaf, ctr)

!       increment tree level
        nlvl = nlvl + 1
        if (nlvl > mlvl) then
          mlvl = 2*mlvl
          call hypoct_malloc2i(lvlx, 2, mlvl+1, .true.)
        endif
        lvlx(:,nlvl+1) = (/ nnode, lvldiv /)
      enddo

!     resize output arrays
      if (nlvl < mlvl) call hypoct_malloc2i(lvlx, 2, nlvl+1, .true.)
      lvlx(2,0) = nlvl
      if (nnode < mnode) then
        call hypoct_malloc1i(xp, nnode+1, .true.)
        call hypoct_malloc2i(nodex, 2, nnode, .true.)
      endif

     end subroutine

!*******************************************************************************
     subroutine hypoct_chld(lvlx, nodex, chldp)
!*******************************************************************************
!    Generate child data.
!
!    Arguments
!    =========
!
!    LVLX : INTEGER, DIMENSION(2,0:*), INTENT(IN)
!      Level data array.
!
!    NODEX : INTEGER, DIMENSION(2,*), INTENT(IN)
!      Node data array.
!
!    CHLDP : INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT)
!      Child pointer array. On exit, CHLDP has dimension NNODE + 1, where NNODE
!      is the total number of nodes. The children of node I have ordered indices
!      CHLDP(I)+1:CHLDP(I+1).
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: lvlx(2,0:*), nodex(2,*)
      integer, allocatable, intent(out) :: chldp(:)

!     local variables
      integer :: nlvl, i, j
!     ==========================================================================

!     initialize
      nlvl = lvlx(2,0)
      allocate(chldp(lvlx(1,nlvl+1)+1))
      chldp = 0

!     count children by adding to parent totals
      do i = 1, nlvl
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          chldp(nodex(1,j)+1) = chldp(nodex(1,j)+1) + 1
        enddo
      enddo

!     compute cumulative sum
      chldp(1) = 1
      do i = 1, lvlx(1,nlvl+1)
        chldp(i+1) = chldp(i) + chldp(i+1)
      enddo

     end subroutine

!*******************************************************************************
     subroutine hypoct_geom(d, lvlx, rootx, nodex, l, ctr)
!*******************************************************************************
!    Generate geometry data.
!
!    Arguments
!    =========
!
!    D : INTEGER, INTENT(IN)
!      Dimension of space. Requires D > 0.
!
!    LVLX : INTEGER, DIMENSION(2,0:*), INTENT(IN)
!      Level data array.
!
!    ROOTX : REAL*8, DIMENSION(2,D), INTENT(IN)
!      Root data array.
!
!    NODEX : INTEGER, DIMENSION(2,*), INTENT(IN)
!      Node data array.
!
!    L : REAL*8, ALLOCATABLE, DIMENSION(:,:), INTENT(OUT)
!      Node extents. On exit, L has dimension (D,NLVL), where NLVL is the tree
!      depth. The extent of a node at level I is L(:,I).
!
!    CTR : REAL*8, ALLOCATABLE, DIMENSION(:,:), INTENT(OUT)
!      Node centers. On exit, CTR has dimension (D,NNODE), where NNODE is the
!      total number of nodes.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, lvlx(2,0:*), nodex(2,*)
      real*8, intent(in) :: rootx(2,d)
      real*8, allocatable, intent(out) :: l(:,:), ctr(:,:)

!     local variables
      character(len=*), parameter :: srname = 'HYPOCT_GEOM'
      integer :: nlvl, nnode, i, j, k
      real*8 :: alpha
      logical :: div(d)
!     ==========================================================================

!     handle input errors
      if (d <= 0) call hypoct_errprm(srname, 'D')

!     initialize
      nlvl  = lvlx(2,0)
      nnode = lvlx(1,nlvl+1)
      allocate(l(d,0:nlvl), ctr(d,nnode))
      l(:,0) = rootx(2,:)
      ctr(:,1) = rootx(1,:)

!     generate geometry data
      div = .false.
      do i = 1, nlvl
        l(:,i) = l(:,i-1)

!       bisect dimensions
        do j = 1, d
          if (ibits(lvlx(2,i+1), j-1, 1) == 0) cycle
          div(j) = .true.
          l(j,i) = 0.5d0*l(j,i)
        enddo

!       shift centers
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          ctr(:,j) = ctr(:,nodex(1,j))
          do k = 1, d
            if (.not. div(k)) cycle
            alpha = ibits(nodex(2,j), k-1, 1) - 0.5d0
            ctr(k,j) = ctr(k,j) + alpha*l(k,i)
          enddo
        enddo
      enddo

     end subroutine

!*******************************************************************************
     subroutine hypoct_ilst(lvlx, xp, nodex, chldp, nbori, nborp, ilsti, ilstp)
!*******************************************************************************
!    Get interaction lists.
!
!    The interaction list of a given node consists of:
!
!    - All nodes at the same level that are children of the neighbors of the
!      node's parent but not neighbors of the node itself.
!
!    - All non-empty nodes at a coarser level (parent or above) that are
!      neighbors of the node's parent but not neighbors of the node itself.
!
!    Arguments
!    =========
!
!    LVLX : INTEGER, DIMENSION(2,0:*), INTENT(IN)
!      Level data array.
!
!    XP : INTEGER, DIMENSION(*), INTENT(IN)
!      Node pointer array.
!
!    NODEX : INTEGER, DIMENSION(2,*), INTENT(IN)
!      Node data array.
!
!    CHLDP : INTEGER, DIMENSION(*), INTENT(IN)
!      Child pointer array.
!
!    NBORP : INTEGER, DIMENSION(*), INTENT(IN)
!      Neighbor pointer array.
!
!    NBORI : INTEGER, DIMENSION(*), INTENT(IN)
!      Neighbor indices.
!
!    ILSTI : INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT)
!      Interaction list indices. On exit, ILSTI has dimension ILSTP(NNODE+1).
!
!    ILSTP : INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT)
!      Interaction list pointer array. On exit, ILSTP has dimension NNODE + 1,
!      where NNODE is the total number of nodes. The nodes in the interation
!      list of node I have ordered indices ILSTI(ILSTP(I)+1:ILSTP(I+1)).
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: lvlx(2,0:*), xp(*), nodex(2,*), chldp(*), &
                             nbori(*), nborp(*)
      integer, allocatable, intent(out) :: ilsti(:), ilstp(:)

!     local variables
      integer :: nlvl, milst, nilst, prnt, inbor, ichld, i, j, k
!     ==========================================================================

!     initialize
      nlvl = lvlx(2,0)
      allocate(ilstp(lvlx(1,nlvl+1)+1))
      ilstp(1:2) = 0
      if (nlvl > 0) then
        do i = lvlx(1,1)+1, lvlx(1,2)
          ilstp(i+1) = 0
        enddo
      endif

!     return if empty
      if (nlvl < 2) then
        allocate(ilsti(0))
        return
      endif

!     get interaction lists
      milst = 1
      allocate(ilsti(milst))
      nilst = 0
      do i = 2, nlvl
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          prnt = nodex(1,j)

!         look through parent-neighbors
          do k = nborp(prnt)+1, nborp(prnt+1)
            inbor = nbori(k)
            if (xp(inbor) < xp(inbor+1)) then
              call hypoct_ilstnb(nbori, nborp, j, inbor, milst, nilst, ilsti)
            endif
          enddo

!         look through children of parent-neighbors
          do k = nborp(prnt)+1, nborp(prnt+1)
            inbor = nbori(k)
            if (inbor <= lvlx(1,i-1)) cycle
            do ichld = chldp(inbor)+1, chldp(inbor+1)
              call hypoct_ilstnb(nbori, nborp, j, ichld, milst, nilst, ilsti)
            enddo
          enddo

!         update number of interactions
          ilstp(j+1) = nilst
        enddo
      enddo

!     resize output arrays
      if (nilst < milst) call hypoct_malloc1i(ilsti, nilst, .true.)
     end subroutine

!*******************************************************************************
     subroutine hypoct_nbor(elem, d, lvlx, xp, nodex, chldp, l, ctr, per, &
                            nbori, nborp)
!*******************************************************************************
!    Find neighbors.
!
!    Let the extension of a node be the spatial region corresponding to all
!    possible point distributions belonging to that node. Then the neighbors of
!    a given node consist of:
!
!    - All nodes at the same level whose extensions are separated from that of
!      the given node by less than the size of the given node's extension.
!
!    - All non-empty nodes at a higher level (parent or coarser) whose
!      extensions are separated from that of the given node by less than the
!      size of the given node's extension.
!
!    In the special case that each node contains only points and not elements
!    (i.e., ELEM = 'p'), this reduces simply to:
!
!    - All nodes at the same level immediately adjoining the given node.
!
!    - All non-empty nodes at a higher level (parent or coarser) immediately
!      adjoining the given node.
!
!    A node is not considered its own neighbor.
!
!    Arguments
!    =========
!
!    ELEM : CHARACTER, INTENT(IN)
!      Element type. Requires ELEM = 'p', 'e', or 's'.
!
!    D : INTEGER, INTENT(IN)
!      Dimension of space. Requires D > 0.
!
!    LVLX : INTEGER, DIMENSION(2,0:*), INTENT(IN)
!      Level data array.
!
!    XP : INTEGER, DIMENSION(*), INTENT(IN)
!      Node pointer array.
!
!    NODEX : INTEGER, DIMENSION(2,*), INTENT(IN)
!      Node data array.
!
!    CHLDP : INTEGER, DIMENSION(*), INTENT(IN)
!      Child pointer array.
!
!    L : REAL*8, DIMENSION(D,*), INTENT(IN)
!      Node extents.
!
!    CTR : REAL*8, DIMENSION(D,*), INTENT(IN)
!      Node centers.
!
!    PER : LOGICAL, DIMENSION(D), INTENT(IN)
!      Periodicity of root node. The domain is periodic in dimension I if
!      PER(I) = .TRUE. Use EXT in HYPOCT_BUILD to control the extent of the
!      root.
!
!    NBORI : INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT)
!      Neighbor indices. On exit, NBORI has dimension NBORP(NNODE+1).
!
!    NBORP : INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT)
!      Neighbor pointer array. On exit, NBORP has dimension NNODE + 1, where
!      NNODE is the total number of nodes. The neighbors of node I have ordered
!      indices NBORI(NBORP(I)+1:NBORP(I+1)).
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: elem
      integer, intent(in) :: d, lvlx(2,0:*), xp(*), nodex(2,*), chldp(*)
      real*8, intent(in) :: l(d,0:*), ctr(d,*)
      logical, intent(in) :: per(d)
      integer, allocatable, intent(out) :: nbori(:), nborp(:)

!     local variables
      character(len=*), parameter :: srname = 'HYPOCT_NBOR'
      integer :: nlvl, nleaf, mnbor, nnbor, nlatt(d), ileaf, inbor, prnt, &
                 i, j, k
      integer, allocatable :: lvlref(:), idx(:,:), idx_(:,:)
      logical :: self
!     ==========================================================================

!     handle input errors
      if ((elem /= 'p') .and. (elem /= 'e') .and. (elem /= 's')) then
        call hypoct_errprm(srname, 'ELEM')
      endif
      if (d <= 0) call hypoct_errprm(srname, 'D')

!     initialize
      nlvl = lvlx(2,0)
      allocate(nborp(lvlx(1,nlvl+1)+1))
      nborp(1:2) = 0

!     return if no neighbors
      if (nlvl == 0) then
        allocate(nbori(0))
        return
      endif

!     allocate work arrays
      nleaf = 0
      do i = 0, nlvl
        nleaf = max(nleaf, lvlx(1,i+1)-lvlx(1,i))
      enddo
      allocate(lvlref(lvlx(1,nlvl+1)), idx(d,nleaf), idx_(d,nleaf))

!     store node levels for reference
      do i = 0, nlvl
        lvlref(lvlx(1,i)+1:lvlx(1,i+1)) = i
      enddo

!     setup neighbor data at level 1
      nleaf = lvlx(1,2) - lvlx(1,1)
      if (xp(1) < xp(2)) then
        nnbor = nleaf
      else
        nnbor = nleaf - 1
      endif
      mnbor = max(nleaf*nnbor, 1)
      allocate(nbori(mnbor))
      do i = lvlx(1,1)+1, lvlx(1,2)
        nborp(i+1) = nborp(i) + nnbor
      enddo
      nnbor = 0
      do i = 1, nleaf
        do j = 1, d
          idx(j,i) = ibits(nodex(2,lvlx(1,1)+i), j-1, 1)
        enddo
        if (xp(1) < xp(2)) then
          nnbor = nnbor + 1
          nbori(nnbor) = 1
        endif
        do j = 1, nleaf
          if (i == j) cycle
          nnbor = nnbor + 1
          nbori(nnbor) = lvlx(1,1) + j
        enddo
      enddo

!     count number of nodes in each dimension
      do i = 1, d
        nlatt(i) = ibits(lvlx(2,2), i-1, 1)
      enddo
      nlatt = nlatt + 1

!     find neighbors
      do i = 2, nlvl

!       update node vector indices
        idx_(:,1:nleaf) = idx(:,1:nleaf)
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          ileaf = j - lvlx(1,i)
          idx(:,ileaf) = idx_(:,nodex(1,j)-lvlx(1,i-1))
          do k = 1, d
            if (ibits(lvlx(2,i+1), k-1, 1) == 0) cycle
            idx(k,ileaf) = 2*idx(k,ileaf)
            if (ibits(nodex(2,j), k-1, 1) == 0) cycle
            idx(k,ileaf) = idx(k,ileaf) + 1
          enddo
        enddo
        nleaf = lvlx(1,i+1) - lvlx(1,i)

!       update maximum vector index values
        do j = 1, d
          if (ibits(lvlx(2,i+1), j-1, 1) == 1) nlatt(j) = 2*nlatt(j)
        enddo

!       look through nodes
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          ileaf = j - lvlx(1,i)
          prnt = nodex(1,j)

!         look through parent and parent-neighbors
          self = .false.
          do k = nborp(prnt)+1, nborp(prnt+1)
            inbor = nbori(k)
            if ((.not. self) .and. (inbor > prnt)) then
              call hypoct_nbornz(xp, prnt, mnbor, nnbor, nbori)
            endif
            if (xp(inbor) == xp(inbor+1)) cycle
            call hypoct_nborcd(elem, d, l, ctr, per, lvlref, j, inbor, &
                               mnbor, nnbor, nbori)
          enddo
          if (.not. self) call hypoct_nbornz(xp, prnt, mnbor, nnbor, nbori)

!         look through children of parent and parent-neighbors
          self = .false.
          do k = nborp(prnt)+1, nborp(prnt+1)
            inbor = nbori(k)
            if (inbor <= lvlx(1,i-1)) cycle
            if ((.not. self) .and. (inbor > prnt)) then
              call hypoct_nborch(elem, d, chldp, per, idx, nlatt, ileaf, prnt, &
                                 lvlx(1,i), mnbor, nnbor, nbori)
              self = .true.
            endif
            call hypoct_nborch(elem, d, chldp, per, idx, nlatt, ileaf, inbor, &
                               lvlx(1,i), mnbor, nnbor, nbori)
          enddo
          if (.not. self) then
            call hypoct_nborch(elem, d, chldp, per, idx, nlatt, ileaf, prnt, &
                               lvlx(1,i), mnbor, nnbor, nbori)
          endif

!         update number of neighbors
          nborp(j+1) = nnbor
        enddo
      enddo

!     resize output arrays
      if (nnbor < mnbor) call hypoct_malloc1i(nbori, nnbor, .true.)

     end subroutine

!*******************************************************************************
     subroutine hypoct_search(elem, d, n, x, siz, mlvl, lvlx, nodex, chldp, l, &
                              ctr, trav)
!*******************************************************************************
!    Search hyperoctree.
!
!    Arguments
!    =========
!
!    ELEM : CHARACTER, INTENT(IN)
!      Element type. Requires ELEM = 'p', 'e', or 's'. If ELEM = 'p', then SIZ
!      is ignored. Larger elements are contained in larger nodes.
!
!    D : INTEGER, INTENT(IN)
!      Dimension of space. Requires D > 0.
!
!    N : INTEGER, INTENT(IN)
!      Number of points to search for. Requires N > 0.
!
!    X : REAL*8, DIMENSION(D,N), INTENT(IN)
!      Point coordinates to search for.
!
!    SIZ : REAL*8, DIMENSION(N), INTENT(IN)
!      Sizes associated with each point. Not accessed if ELEM = 'p'.
!
!    MLVL : INTEGER, INTENT(IN)
!      Maximum tree depth to search. Requires MLVL >= 0.
!
!    LVLX : INTEGER, DIMENSION(2,0:*), INTENT(IN)
!      Level data array.
!
!    NODEX : INTEGER, DIMENSION(2,*), INTENT(IN)
!      Node data array.
!
!    CHLDP : INTEGER, DIMENSION(*), INTENT(IN)
!      Child pointer array.
!
!    L : REAL*8, DIMENSION(D,*), INTENT(IN)
!      Node extents.
!
!    CTR : REAL*8, DIMENSION(D,*), INTENT(IN)
!      Node centers.
!
!    TRAV : INTEGER, DIMENSION(N,0:MLVL), INTENT(OUT)
!      Tree traversal array. The node containing point I at level J has index
!      TRAV(I,J); if no such node exists, then TRAV(I,J) = 0.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: elem
      integer, intent(in) :: d, n, mlvl, lvlx(2,0:*), nodex(2,*), chldp(*)
      real*8, intent(in) :: x(d,n), siz(n), l(d,0:*), ctr(d,*)
      integer, intent(out) :: trav(n,0:mlvl)

!     local variables
      character(len=*), parameter :: srname = 'HYPOCT_SEARCH'
      integer :: id, i, j, k
      real*8 :: ext(2,d)
      logical :: found
!     ==========================================================================

!     handle input errors
      if ((elem /= 'p') .and. (elem /= 'e') .and. (elem /= 's')) then
        call hypoct_errprm(srname, 'ELEM')
      endif
      if (d <= 0) call hypoct_errprm(srname, 'D')
      if (n <= 0) call hypoct_errprm(srname, 'N')
      if (mlvl < 0) call hypoct_errprm(srname, 'MLVL')

!     initialize
      ext(1,:) = ctr(:,1) - 0.5d0*l(:,0)
      ext(2,:) = ctr(:,1) + 0.5d0*l(:,0)

!     loop through points
      do i = 1, n

!       check if in root
        if ((.not. all((ext(1,:) <= x(:,i)) .and. (x(:,i) <= ext(2,:)))) .or. &
            hypoct_big(elem, siz(i), minval(l(:,0)))) then
          trav(i,:) = 0
          cycle
        endif

!       search children recursively
        trav(i,0) = 1
        do j = 1, mlvl
          id = hypoct_chldid(d, x(1,i), ctr(1,trav(i,j-1)), lvlx(2,j+1))
          found = .false.
          do k = chldp(trav(i,j-1))+1, chldp(trav(i,j-1)+1)
            if (nodex(2,k) >  id) exit
            if (nodex(2,k) == id) then
              trav(i,j) = k
              found = .true.
              exit
            endif
          enddo
          if ((.not. found) .or. &
              (found .and. hypoct_big(elem, siz(i), minval(l(:,j))))) then
            trav(i,j:) = 0
            exit
          endif
        enddo
      enddo

     end subroutine

!===============================================================================
!    Internal routines
!===============================================================================

!*******************************************************************************
     function hypoct_big(elem, siz, lmin) result(big)
!*******************************************************************************
!    Determine if a point is too big for the next finer level based on its size.
!
!    If ELEM = 'p', the point is never too big. If ELEM = 'e', the point is too
!    big if it is greater than a quarter of the node's size. If ELEM = 's', the
!    point is too big if it is greater than half of the node's size.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character :: elem
      real*8 :: siz, lmin
      logical :: big
!     ==========================================================================

      big = .false.
      if (elem == 'p') then
        return
      elseif (elem == 'e') then
        big = (4d0*siz > lmin)
      elseif (elem == 's') then
        big = (2d0*siz > lmin)
      endif

     end function

!*******************************************************************************
     function hypoct_chldid(d, x, ctr, lvldiv) result(id)
!*******************************************************************************
!    Compute child ID of child node containing a given point in its parent.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer :: d, lvldiv, id
      real*8 :: x(d), ctr(d)

!     local variables
      integer :: i
!     ==========================================================================

      id = 0
      do i = 1, d
        if (ibits(lvldiv, i-1, 1) == 0) cycle
        if (x(i) > ctr(i)) id = ibset(id, i-1)
      enddo

     end function

!*******************************************************************************
     subroutine hypoct_errmsg(msg)
!*******************************************************************************
!    Print error message and terminate.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character(len=*), intent(in) :: msg
!     ==========================================================================

      print 10, msg
10    format(' ** ', a)

      stop

     end subroutine

!*******************************************************************************
     subroutine hypoct_errprm(srname, prm)
!*******************************************************************************
!    Print illegal input error message and terminate.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character srname*(*), prm*(*)
!     ==========================================================================

      call hypoct_errmsg('On entry to ' // srname // ', parameter ' // prm // &
                         ' had an illegal value.')

     end subroutine

!*******************************************************************************
     subroutine hypoct_hold(elem, siz, lmin, nleaf, leaf, div, xi, xp)
!*******************************************************************************
!    Hold points at current leaf level and sort by hold status. A point is held
!    if the node to which it is assigned will not be further subdivided or if it
!    is too big for the next level.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: elem
      integer, intent(in) :: nleaf
      real*8, intent(in) :: siz(*), lmin
      integer, intent(inout) :: leaf(nleaf+1), xi(leaf(nleaf+1)), xp(nleaf+1)
      logical, intent(inout) :: div(nleaf)

!     local variables
      integer :: ptr(2,nleaf), xi_(leaf(nleaf+1)), i, j, k
      logical :: hold(leaf(nleaf+1))
!     ==========================================================================

!     count points to hold
      xp(2:nleaf+1) = 0
      do i = 1, nleaf
        if (.not. div(i)) then
          hold(leaf(i)+1:leaf(i+1)) = .true.
        else
          do j = leaf(i)+1, leaf(i+1)
            hold(j) = hypoct_big(elem, siz(xi(j)), lmin)
          enddo
          if (all(hold(leaf(i)+1:leaf(i+1)))) div(i) = .false.
        endif
        do j = leaf(i)+1, leaf(i+1)
          if (hold(j)) xp(i+1) = xp(i+1) + 1
        enddo
      enddo
      do i = 1, nleaf
        xp(i+1) = xp(i) + xp(i+1)
      enddo

!     sort points by hold status
      ptr(1,:) = xp(1:nleaf) - xp(1)
      ptr(2,:) = leaf(1:nleaf) - xp(1:nleaf) + xp(nleaf+1)
      xi_ = xi
      do i = 1, nleaf
        do j = leaf(i)+1, leaf(i+1)
          if (hold(j)) then
            ptr(1,i) = ptr(1,i) + 1
            k = ptr(1,i)
          else
            ptr(2,i) = ptr(2,i) + 1
            k = ptr(2,i)
          endif
          xi(k) = xi_(j)
        enddo
      enddo

!     update leaf data
      leaf = leaf - xp
      leaf = leaf - leaf(1)

     end subroutine

!*******************************************************************************
     subroutine hypoct_ilstnb(nbori, nborp, inode, icand, milst, nilst, ilsti)
!*******************************************************************************
!    Add node to interaction list if the candidate is not a neighbor.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: nbori(*), nborp(*), inode, icand
      integer, intent(inout) :: milst, nilst
      integer, allocatable, intent(inout) :: ilsti(:)

!     local variables
      integer :: inbor, i
      logical :: malloc
!     ==========================================================================

!     check if neighbor
      do i = nborp(inode)+1, nborp(inode+1)
        inbor = nbori(i)
        if (inbor >  icand) exit
        if (inbor == icand) return
      enddo

!     add to interaction list
      nilst = nilst + 1
      malloc = .false.
      do while (nilst > milst)
        malloc = .true.
        milst = 2*milst
      enddo
      if (malloc) call hypoct_malloc1i(ilsti, milst, .true.)
      ilsti(nilst) = icand

     end subroutine

!*******************************************************************************
     subroutine hypoct_malloc1i(a, ub, copy)
!*******************************************************************************
!    Resize and copy 1D integer allocatable arrays.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: ub
      logical, intent(in) :: copy
      integer, allocatable, intent(inout) :: a(:)

!     local variables
      integer :: l, u
      integer, allocatable :: b(:)
!     ==========================================================================

      l = lbound(a, 1)

!     allocate but don't copy
      if (.not. copy) then
        deallocate(a)
        allocate(a(l:ub))
        return
      endif

!     allocate and copy
      u = min(ubound(a,1), ub)
      allocate(b(l:u))
      b = a(l:u)
      deallocate(a)
      allocate(a(l:ub))
      a(l:u) = b

     end subroutine

!*******************************************************************************
     subroutine hypoct_malloc1l(a, ub, copy)
!*******************************************************************************
!    Resize and copy 1D logical allocatable arrays.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: ub
      logical, intent(in) :: copy
      logical, allocatable, intent(inout) :: a(:)

!     local variables
      integer :: l, u
      logical, allocatable :: b(:)
!     ==========================================================================

      l = lbound(a, 1)

!     allocate but don't copy
      if (.not. copy) then
        deallocate(a)
        allocate(a(l:ub))
        return
      endif

!     allocate and copy
      u = min(ubound(a,1), ub)
      allocate(b(l:u))
      b = a(l:u)
      deallocate(a)
      allocate(a(l:ub))
      a(l:u) = b

     end subroutine

!*******************************************************************************
     subroutine hypoct_malloc2d(a, ub1, ub2, copy)
!*******************************************************************************
!    Resize and copy 2D real allocatable arrays.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: ub1, ub2
      logical, intent(in) :: copy
      real*8, allocatable, intent(inout) :: a(:,:)

!     local variables
      integer :: l1, l2, u1, u2
      real*8, allocatable :: b(:,:)
!     ==========================================================================

      l1 = lbound(a, 1)
      l2 = lbound(a, 2)

!     allocate but don't copy
      if (.not. copy) then
        deallocate(a)
        allocate(a(l1:ub1,l2:ub2))
        return
      endif

!     allocate and copy
      u1 = min(ubound(a,1), ub1)
      u2 = min(ubound(a,2), ub2)
      allocate(b(l1:u1,l2:u2))
      b = a(l1:u1,l2:u2)
      deallocate(a)
      allocate(a(l1:ub1,l2:ub2))
      a(l1:u1,l2:u2) = b

     end subroutine

!*******************************************************************************
     subroutine hypoct_malloc2i(a, ub1, ub2, copy)
!*******************************************************************************
!    Resize and copy 2D integer allocatable arrays.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: ub1, ub2
      logical, intent(in) :: copy
      integer, allocatable, intent(inout) :: a(:,:)

!     local variables
      integer :: l1, l2, u1, u2
      integer, allocatable :: b(:,:)
!     ==========================================================================

      l1 = lbound(a, 1)
      l2 = lbound(a, 2)

!     allocate but don't copy
      if (.not. copy) then
        deallocate(a)
        allocate(a(l1:ub1,l2:ub2))
        return
      endif

!     allocate and copy
      u1 = min(ubound(a,1), ub1)
      u2 = min(ubound(a,2), ub2)
      allocate(b(l1:u1,l2:u2))
      b = a(l1:u1,l2:u2)
      deallocate(a)
      allocate(a(l1:ub1,l2:ub2))
      a(l1:u1,l2:u2) = b

     end subroutine

!*******************************************************************************
     subroutine hypoct_nborcd(elem, d, l, ctr, per, lvlref, ileaf, inbor, &
                              mnbor, nnbor, nbori)
!*******************************************************************************
!    Add neighbor from a coarser level if it is sufficiently close.
!
!    See below for details.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: elem
      integer, intent(in) :: d, lvlref(*), ileaf, inbor
      real*8, intent(in) :: l(d,0:*), ctr(d,*)
      logical, intent(in) :: per(d)
      integer, intent(inout) :: mnbor, nnbor
      integer, allocatable, intent(inout) :: nbori(:)

!     local variables
      integer :: i
      real*8 :: dist(d), lleaf(d), lnbor(d)
      logical :: malloc
!     ==========================================================================

!     find distance between nodes
      dist = abs(ctr(:,ileaf) - ctr(:,inbor))
      do i = 1, d
        if (per(i)) dist(i) = min(dist(i), abs(dist(i) - l(i,0)))
      enddo

!     check if sufficiently close
!     --------------------------------------------------------------------------
!     The formulas below come from the following. Two nodes are neighbors if and
!     only if they are sufficiently close along all dimensions. Hence, we can
!     examine each dimension and see if the nodes are too far apart along that
!     dimension--if so, then we can reject that they are neighbors. Thus,
!     consider each dimension independently and rescale so that along that
!     dimension, the size of the smaller node is 1 while that of the larger node
!     is 2^L, where L >= 0 is an integer. Let D be the distance between the node
!     centers. Clearly, we cannot reject if D = 0, so assume hereafter that
!     D > 0. Then M = D - (1/2)*(1 + 2^L) is the distance between the node
!     boundaries and is also an integer since it must be a multiple of the
!     smaller box size.
!
!     If ELEM = 'p', the nodes are not neighbors if M > 0 or, equivalently,
!     M >= 1. To allow a sufficient margin for roundoff error, we rewrite this
!     as M > 1/2 or D > 1 + (1/2)*2^L.
!
!     If ELEM = 'e', the nodes are not neighbors if M - (1/4)*(1+ 2^L) > 3/2,
!     i.e., 4*M > 7 + 2^L or 4*M >= 8 + 2^L. We write this as 4*M > 15/2 + 2^L
!     or D > 19/8 + (3/4)*2^L.
!
!     If ELEM = 's', the nodes are not neighbors if M - (1/2)*(1 + 2^L) > 0,
!     i.e., 2*M > 1 + 2^L or 2*M >= 2 + 2^L. We write this as 2*M > 3/2 + 2^L
!     or D > 5/4 + 2^L.
!     --------------------------------------------------------------------------
      lleaf = l(:,lvlref(ileaf))
      lnbor = l(:,lvlref(inbor))
      do i = 1, d
        if (elem == 'p') then
          if (dist(i) > lleaf(i) + 0.5d0*lnbor(i)) return
        elseif (elem == 'e') then
          if (dist(i) > 2.375d0*lleaf(i) + 0.75d0*lnbor(i)) return
        elseif (elem == 's') then
          if (dist(i) > 1.25d0*lleaf(i) + lnbor(i)) return
        endif
      enddo

!     add to neighbors
      nnbor = nnbor + 1
      malloc = .false.
      do while (nnbor > mnbor)
        malloc = .true.
        mnbor = 2*mnbor
      enddo
      if (malloc) call hypoct_malloc1i(nbori, mnbor, .true.)
      nbori(nnbor) = inbor

     end subroutine

!*******************************************************************************
     subroutine hypoct_nborch(elem, d, chldp, per, idx, nlatt, ileaf, prnt, &
                              lvln, mnbor, nnbor, nbori)
!*******************************************************************************
!    Find neighbors among children of a given node at the next coarser level.
!
!    Note that these children are at the same level as the potential node which
!    they neighbor.
!
!    If ELEM = 'p' or 's', the neighbors consist of those nodes that are at most
!    "one over" from the node in question. If ELEM = 'e', the neighbors consist
!    of those nodes that are at most "two over".
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: elem
      integer, intent(in) :: d, chldp(*), idx(d,*), nlatt(d), ileaf, prnt, lvln
      logical, intent(in) :: per(d)
      integer, intent(inout) :: mnbor, nnbor
      integer, allocatable, intent(inout) :: nbori(:)

!     local variables
      integer :: jleaf, dist, i, j
      logical :: skip, malloc
!     ==========================================================================

!     loop through children
      do i = chldp(prnt)+1, chldp(prnt+1)
        jleaf = i - lvln

!       ignore if same node
        if (ileaf == jleaf) cycle

!       check if neighbor by comparing integer indices
        skip = .false.
        do j = 1, d
          dist = abs(idx(j,ileaf) - idx(j,jleaf))
          if (per(j)) dist = min(dist, abs(dist - nlatt(j)))
          if ((elem == 'p') .or. (elem == 's')) then
            if (dist > 1) then
              skip = .true.
              exit
            endif
          elseif (elem == 'e') then
            if (dist > 2) then
              skip = .true.
              exit
            endif
          endif
        enddo
        if (skip) cycle

!       add to neighbors
        nnbor = nnbor + 1
        malloc = .false.
        do while (nnbor > mnbor)
          malloc = .true.
          mnbor = 2*mnbor
        enddo
        if (malloc) call hypoct_malloc1i(nbori, mnbor, .true.)
        nbori(nnbor) = i
      enddo

     end subroutine

!*******************************************************************************
     subroutine hypoct_nbornz(xp, node, mnbor, nnbor, nbori)
!*******************************************************************************
!    Add to neighbors if nonempty (used to add parent).
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: xp(*), node
      integer, intent(inout) :: mnbor, nnbor
      integer, allocatable, intent(inout) :: nbori(:)

!     local variables
      logical :: malloc
!     ==========================================================================

!     return if empty
      if (xp(node) == xp(node+1)) return

!     add to neighbors
      nnbor = nnbor + 1
      malloc = .false.
      do while (nnbor > mnbor)
        malloc = .true.
        mnbor = 2*mnbor
      enddo
      if (malloc) call hypoct_malloc1i(nbori, mnbor, .true.)
      nbori(nnbor) = node

     end subroutine

!*******************************************************************************
     subroutine hypoct_subdiv(d, x, lvldiv, prnt, ctr, n, xi, mnode, nnode, &
                              xp, nodex)
!*******************************************************************************
!    Subdivide leaves and sort.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, lvldiv, prnt, n
      real*8, intent(in) :: x(d,*), ctr(d)
      integer, intent(inout) :: xi(n), mnode, nnode
      integer, allocatable, intent(inout) :: xp(:), nodex(:,:)

!     local variables
      integer :: nchld, id(n), xi_(n), i, j, k
      integer, allocatable :: ptr(:)
      logical :: new, malloc
!     ==========================================================================

!     find new leaves based on child information
      nchld = 0
      do i = 1, n
        id(i) = hypoct_chldid(d, x(1,xi(i)), ctr, lvldiv)
        new = .true.
        do j = 1, nchld
          if (nodex(2,nnode+j) >  id(i)) exit
          if (nodex(2,nnode+j) == id(i)) then
            new = .false.
            exit
          endif
        enddo
        if (new) then
          nchld = nchld + 1
          malloc = .false.
          do while (nnode + nchld > mnode)
            malloc = .true.
            mnode = 2*mnode
          enddo
          if (malloc) then
            call hypoct_malloc1i(xp, mnode+1, .true.)
            call hypoct_malloc2i(nodex, 2, mnode, .true.)
          endif
          do k = nchld-1, j, -1
            xp(nnode+k+2) = xp(nnode+k+1)
            nodex(1,nnode+k+1) = nodex(1,nnode+k)
            nodex(2,nnode+k+1) = nodex(2,nnode+k)
          enddo
          xp(nnode+j+1) = 1
          nodex(:,nnode+j) = (/ prnt, id(i) /)
        else
          xp(nnode+j+1) = xp(nnode+j+1) + 1
        endif
      enddo
      do i = 1, nchld
        xp(nnode+i+1) = xp(nnode+i) + xp(nnode+i+1)
      enddo

!     sort points according to new leaves
      allocate(ptr(nchld))
      ptr = xp(nnode+1:nnode+nchld) - xp(nnode+1)
      xi_ = xi
      do i = 1, n
        do j = 1, nchld
          if (nodex(2,nnode+j) == id(i)) exit
        enddo
        ptr(j) = ptr(j) + 1
        xi(ptr(j)) = xi_(i)
      enddo

!     update number of nodes
      nnode = nnode + nchld

     end subroutine

!*******************************************************************************
     subroutine hypoct_update(lvlp, xp, nodex, d, l, lvldiv, n, leaf, ctr)
!*******************************************************************************
!    Update leaf data.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: lvlp, xp(*), nodex(2,*), d, lvldiv, n
      real*8, intent(in) :: l(d)
      integer, intent(inout) :: leaf(n+1)
      real*8, intent(inout) :: ctr(d,*)

!     local variables
      integer :: id, i, j
      real*8 :: ctr_(d,n)
!     ==========================================================================

!     copy centers of parents
      do i = 1, n
        ctr_(:,i) = ctr(:,nodex(1,i)-lvlp)
      enddo
      ctr(:,1:n) = ctr_

!     update number of leaves
      leaf = xp(1:n+1) - xp(1)

!     update leaf centers
      do i = 1, n
        id = nodex(2,i)
        do j = 1, d
          if (ibits(lvldiv, j-1, 1) == 1) then
            if (ibits(id, j-1, 1) == 0) then
              ctr(j,i) = ctr(j,i) - 0.5d0*l(j)
            else
              ctr(j,i) = ctr(j,i) + 0.5d0*l(j)
            endif
          endif
        enddo
      enddo

     end subroutine

    end module