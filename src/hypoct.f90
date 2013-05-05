!*******************************************************************************
!   Copyright (C) 2013 Kenneth L. Ho
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
!   - Support for general elements (e.g., triangles) by associating with each
!     point (e.g., centroid) a size (e.g., diameter). Larger elements are
!     assigned to larger nodes so as to enforce the well-separated condition for
!     non-self non-neighbors.
!
!   - Compatibility with point-point, point-element (collocation/qualocation),
!     and element-element (Galerkin) interactions.
!
!   - Optimizations for non-uniform and high-dimensional data such as adaptive
!     subdivision (of both nodes and levels) and pruning of empty leaves.
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
     subroutine hypoct_build(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
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
!
!    INTR : CHARACTER, INTENT(IN)
!      Interaction type:
!        INTR = 'p': point-point
!        INTR = 'c': point-element (collocation or qualocation)
!        INTR = 'g': element-element (Galerkin)
!      The interaction type controls how points are assigned to nodes according
!      to their sizes. If INTR = 'p', then SIZ is ignored.
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
!      Sizes associated with each point (e.g., triangle diameters with triangle
!      centroids). Not accessed if INTR = 'p'.
!
!    OCC : INTEGER, INTENT(IN)
!      Maximum leaf occupancy.
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
      character, intent(in) :: adap, intr
      integer, intent(in) :: d, n, occ, lvlmax
      real*8, intent(in) :: x(d,n), siz(n), ext(d)
      integer, intent(out) :: xi(n)
      integer, allocatable, intent(out) :: lvlx(:,:), xp(:), nodex(:,:)
      real*8, intent(out) :: rootx(2,d)

!     local variables
      character(len=*), parameter :: srname = 'HYPOCT_BUILD'
      integer :: mlvl, nlvl, mnode, nnode, nnode_, mleaf, nleaf, lvldiv, i
      integer, allocatable :: leaf(:)
      real*8, parameter :: divrat = 1d0 / sqrt(2d0)
      real*8 :: xrng(2), l(d), lrng(2)
      real*8, allocatable :: ctr(:,:)
      logical :: malloc
      logical, allocatable :: div(:)
!     ==========================================================================

!     handle input errors
      if ((adap /= 'a') .and. (adap /= 'u')) then
        call hypoct_errprm(srname, 'ADAP')
      endif
      if ((intr /= 'p') .and. (intr /= 'c') .and. (intr /= 'g')) then
        call hypoct_errprm(srname, 'INTR')
      endif
      if (d <= 0) call hypoct_errprm(srname, 'D')
      if (n <= 0) call hypoct_errprm(srname, 'N')

!     initialize
      if (lvlmax >= 0) then
        mlvl = lvlmax
      else
        mlvl = 1
      endif
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

!       divide sides
        lvldiv = 0
        do i = 1, d
          if (l(i) > divrat*lrng(2)) then
            lvldiv = ibset(lvldiv, i-1)
            l(i) = 0.5d0*l(i)
          endif
        enddo
        lrng = (/ minval(l), maxval(l) /)

!       hold points if appropriate
        call hypoct_hold(intr, siz, lrng(1), nleaf, leaf, div, &
                         xi(xp(lvlx(1,nlvl)+1)+1), xp(lvlx(1,nlvl)+1))

!       stop if none left
        if (leaf(nleaf+1) == 0) exit

!       divide leaves
        nnode_ = nnode
        do i = 1, nleaf
          if (div(i)) then
            call hypoct_subdiv(d, x, lvldiv, lvlx(1,nlvl)+i, ctr(1,i), &
                               leaf(i+1)-leaf(i), &
                               xi(xp(lvlx(1,nlvl+1)+1)+leaf(i)+1), &
                               mnode, nnode, xp, nodex)
          endif
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
        if ((lvlmax < 0) .and. (nlvl > mlvl)) then
          mlvl = 2*mlvl
          call hypoct_malloc2i(lvlx, 2, mlvl+1, .true.)
        endif
        lvlx(:,nlvl+1) = (/ nnode, lvldiv /)
      enddo

!     resize output arrays
      if (nlvl < mlvl) then
        call hypoct_malloc2i(lvlx, 2, nlvl+1, .true.)
      endif
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

!     count number of children for each node
      do i = 1, nlvl
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          chldp(nodex(1,j)+1) = chldp(nodex(1,j)+1) + 1
        enddo
      enddo

!     do cumulative sum
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
!      Dimension of space.
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
      integer :: nlvl, nnode, i, j, k
      logical :: div(d)
!     ==========================================================================

!     initialize
      nlvl  = lvlx(2,0)
      nnode = lvlx(1,nlvl+1)
      allocate(l(d,0:nlvl), ctr(d,nnode))
      l(:,0) = rootx(2,:)
      ctr(:,1) = rootx(1,:)

!     generate geometry data
      do i = 1, nlvl
        l(:,i) = l(:,i-1)
        do j = 1, d
          if (ibits(lvlx(2,i+1), j-1, 1) == 1) then
            div(j) = .true.
            l(j,i) = 0.5d0*l(j,i)
          endif
        enddo
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          ctr(:,j) = ctr(:,nodex(1,j))
          do k = 1, d
            if (div(k)) then
              if (ibits(nodex(2,j), k-1, 1) == 0) then
                ctr(k,j) = ctr(k,j) - 0.5d0*l(k,i)
              else
                ctr(k,j) = ctr(k,j) + 0.5d0*l(k,i)
              endif
            endif
          enddo
        enddo
      enddo

     end subroutine

!*******************************************************************************
     subroutine hypoct_ilst(lvlx, nodex, chldp, nbori, nborp, ilsti, ilstp)
!*******************************************************************************
!    Get interaction lists.
!
!    The interaction list of a given node consists of those nodes at the same
!    level that are the children of its parent's neighbors but which are not
!    themselves neighbors of the node.
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
      integer, intent(in) :: lvlx(2,0:*), nodex(2,*), chldp(*), nbori(*), &
                             nborp(*)
      integer, allocatable, intent(out) :: ilsti(:), ilstp(:)

!     local variables
      integer :: nlvl, milst, nilst, ilvl, inode, inbor, ichld, prnt, i, j
      logical :: intr, malloc
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

!     get interaction lists
      milst = 0
      allocate(ilsti(milst))
      nilst = 0
      do ilvl = 2, nlvl
        do inode = lvlx(1,ilvl)+1, lvlx(1,ilvl+1)
          prnt = nodex(1,inode)
          do i = nborp(prnt)+1, nborp(prnt+1)
            inbor = nbori(i)
            if (inbor <= lvlx(1,ilvl-1)) cycle
            do ichld = chldp(inbor)+1, chldp(inbor+1)
              intr = .true.
              do j = nborp(inode)+1, nborp(inode+1)
                if (nbori(j) >  ichld) exit
                if (nbori(j) == ichld) then
                  intr = .false.
                  exit
                endif
              enddo
              if (intr) then
                nilst = nilst + 1
                malloc = .false.
                if (milst == 0) then
                  malloc = .true.
                  milst = 1
                endif
                do while (nilst > milst)
                  malloc = .true.
                  milst = 2*milst
                enddo
                if (malloc) call hypoct_malloc1i(ilsti, milst, .true.)
                ilsti(nilst) = ichld
              endif
            enddo
          enddo
          ilstp(inode+1) = nilst
        enddo
      enddo

!     resize output arrays
      if (nilst < milst) then
        call hypoct_malloc1i(ilsti, nilst, .true.)
      endif
     end subroutine

!*******************************************************************************
     subroutine hypoct_nbor(d, lvlx, xp, nodex, chldp, per, nbori, nborp)
!*******************************************************************************
!    Find neighbors.
!
!    The neighbors of a given node are those nodes at the same level or higher
!    that are nonempty and within one of the nodes' sizes of each other. A node
!    is not considered its own neighbor.
!
!    Arguments
!    =========
!
!    D : INTEGER, INTENT(IN)
!      Dimension of space.
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
      integer, intent(in) :: d, lvlx(2,0:*), xp(*), nodex(2,*), chldp(*)
      logical, intent(in) :: per(d)
      integer, allocatable, intent(out) :: nbori(:), nborp(:)

!     local variables
      integer :: nlvl, nleaf, mnbor, nnbor, nnbor_, nlatt(d), ileaf, inbor, &
                 prnt, i, j, k
      integer, allocatable :: idx(:,:), idx_(:,:)
      logical :: self, malloc
!     ==========================================================================

!     initialize
      nlvl = lvlx(2,0)
      nleaf = 0
      do i = 0, nlvl
        nleaf = max(nleaf, lvlx(1,i+1)-lvlx(1,i))
      enddo
      allocate(nborp(lvlx(1,nlvl+1)+1), idx(d,nleaf), idx_(d,nleaf))
      nborp(1:2) = 0
      if (nlvl > 0) then
        nleaf = lvlx(1,2) - lvlx(1,1)
        if (xp(1) < xp(2)) then
          nnbor = nleaf
        else
          nnbor = nleaf - 1
        endif
        mnbor = nleaf*nnbor
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
            if (i /= j) then
              nnbor = nnbor + 1
              nbori(nnbor) = lvlx(1,1) + j
            endif
          enddo
        enddo
        if (any(per)) then
          do i = 1, d
            nlatt(i) = ibits(lvlx(2,2), i-1, 1)
          enddo
          nlatt = nlatt + 1
        endif
      else
        mnbor = 0
        allocate(nbori(mnbor))
      endif

!     find neighbors
      do i = 2, nlvl

!       update node vector indices
        idx_(:,1:nleaf) = idx(:,1:nleaf)
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          ileaf = j - lvlx(1,i)
          idx(:,ileaf) = idx_(:,nodex(1,j)-lvlx(1,i-1))
          do k = 1, d
            if (ibits(lvlx(2,i+1), k-1, 1) == 1) then
              idx(k,ileaf) = 2*idx(k,ileaf)
              if (ibits(nodex(2,j), k-1, 1) == 1) then
                idx(k,ileaf) = idx(k,ileaf) + 1
              endif
            endif
          enddo
        enddo
        nleaf = lvlx(1,i+1) - lvlx(1,i)

!       update maximum vector index values
        if (any(per)) then
          do j = 1, d
            if (ibits(lvlx(2,i+1), j-1, 1) == 1) then
              nlatt(j) = 2*nlatt(j)
            endif
          enddo
        endif

!       look through nodes
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          ileaf = j - lvlx(1,i)
          prnt = nodex(1,j)

!         allocate storage for parent and parent-neighbors
          nnbor_ = nnbor
          if (xp(prnt) < xp(prnt+1)) nnbor_ = nnbor_ + 1
          do k = nborp(prnt)+1, nborp(prnt+1)
            inbor = nbori(k)
            if (inbor <= lvlx(1,i-1)) then
              nnbor_ = nnbor_ + 1
            else
              if (xp(inbor) < xp(inbor+1)) then
                nnbor_ = nnbor_ + 1
              endif
            endif
          enddo
          malloc = .false.
          if (mnbor == 0) then
            malloc = .true.
            mnbor = 1
          endif
          do while (nnbor_ > mnbor)
            malloc = .true.
            mnbor = 2*mnbor
          enddo
          if (malloc) call hypoct_malloc1i(nbori, mnbor, .true.)

!         look through parent and parent-neighbors
          self = .false.
          do k = nborp(prnt)+1, nborp(prnt+1)
            inbor = nbori(k)
            if (inbor <= lvlx(1,i-1)) then
              nnbor = nnbor + 1
              nbori(nnbor) = inbor
            else
              if (xp(inbor) < xp(inbor+1)) then
                if ((xp(prnt) < xp(prnt+1)) .and. (.not. self) &
                                            .and. (inbor > prnt)) then
                  nnbor = nnbor + 1
                  nbori(nnbor) = prnt
                  self = .true.
                endif
                nnbor = nnbor + 1
                nbori(nnbor) = inbor
              endif
            endif
          enddo
          if ((xp(prnt) < xp(prnt+1)) .and. (.not. self)) then
            nnbor = nnbor + 1
            nbori(nnbor) = prnt
          endif

!         look through children of parent and parent-neighbors
          self = .false.
          do k = nborp(prnt)+1, nborp(prnt+1)
            inbor = nbori(k)
            if (inbor <= lvlx(1,i-1)) cycle
            if ((.not. self) .and. (inbor > prnt)) then
              call hypoct_nborch(d, chldp, per, idx, nlatt, ileaf, prnt, &
                                 lvlx(1,i), mnbor, nnbor, nbori)
              self = .true.
            endif
            call hypoct_nborch(d, chldp, per, idx, nlatt, ileaf, inbor, &
                               lvlx(1,i), mnbor, nnbor, nbori)
          enddo
          if (.not. self) then
            call hypoct_nborch(d, chldp, per, idx, nlatt, ileaf, prnt, &
                               lvlx(1,i), mnbor, nnbor, nbori)
          endif

!         update number of neighbors
          nborp(j+1) = nnbor
        enddo
      enddo

!     resize output arrays
      if (nnbor < mnbor) then
        call hypoct_malloc1i(nbori, nnbor, .true.)
      endif

     end subroutine

!*******************************************************************************
     subroutine hypoct_search(d, n, x, mlvl, lvlx, rootx, nodex, chldp, ctr, &
                              trav)
!*******************************************************************************
!    Search hyperoctree.
!
!    Arguments
!    =========
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
!    MLVL : INTEGER, INTENT(IN)
!      Maximum tree depth to search. Requires MLVL >= 0.
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
!    CHLDP : INTEGER, DIMENSION(*), INTENT(IN)
!      Child pointer array.
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
      integer, intent(in) :: d, n, mlvl, lvlx(2,0:*), nodex(2,*), chldp(*)
      real*8, intent(in) :: x(d,n), rootx(2,d), ctr(d,*)
      integer, intent(out) :: trav(n,0:mlvl)

!     local variables
      character(len=*), parameter :: srname = 'HYPOCT_SEARCH'
      integer :: id, i, j, k
      real*8 :: ext(2,d)
      logical :: found
!     ==========================================================================

!     handle input errors
      if (d <= 0) call hypoct_errprm(srname, 'D')
      if (n <= 0) call hypoct_errprm(srname, 'N')
      if (mlvl < 0) call hypoct_errprm(srname, 'MLVL')

!     initialize
      ext(1,:) = ctr(:,1) - 0.5d0*rootx(2,:)
      ext(2,:) = ctr(:,1) + 0.5d0*rootx(2,:)

!     loop through points
      do i = 1, n

!       check if in root
        if (.not. all((ext(1,:) <= x(:,i)) .and. (x(:,i) <= ext(2,:)))) then
          trav(i,:) = 0
          cycle
        endif

!       search children recursively
        trav(i,0) = 1
        do j = 1, mlvl
          id = hypoct_chldid(d, x(1,i), ctr(1,trav(i,j-1)), lvlx(2,j+1))
          found = .false.
          do k = chldp(trav(i,j-1))+1, chldp(trav(i,j-1)+1)
            if (nodex(2,k) == id) then
              trav(i,j) = k
              found = .true.
              exit
            endif
          enddo
          if (.not. found) then
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
     function hypoct_big(intr, siz, lmin) result(big)
!*******************************************************************************
!    Determine if a point is too big for the next level based on its size.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character :: intr
      real*8 :: siz, lmin
      logical :: big

!     local variables
      real*8 :: bigrat
!     ==========================================================================

!     treat point interactions
      if (intr == 'p') then
        big = .false.
        return
      endif

!     treat others
      bigrat = 0
      if (intr == 'c') then
        bigrat = 2
      elseif (intr == 'g') then
        bigrat = 3
      endif
      big = (bigrat*siz > lmin)

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
        if (ibits(lvldiv, i-1, 1) == 1) then
          if (x(i) > ctr(i)) id = ibset(id, i-1)
        endif
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
     subroutine hypoct_hold(intr, siz, lmin, nleaf, leaf, div, xi, xp)
!*******************************************************************************
!    Hold points at current leaf level and sort by hold status. A point is held
!    if the node to which it is assigned will not be further subdivided or it is
!    too big for the next level.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: intr
      real*8, intent(in) :: siz(*), lmin
      integer, intent(inout) :: nleaf, leaf(nleaf+1), xi(leaf(nleaf+1)), &
                                xp(nleaf+1)
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
            hold(j) = hypoct_big(intr, siz(xi(j)), lmin)
          enddo
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
     subroutine hypoct_nborch(d, chldp, per, idx, nlatt, ileaf, prnt, lvln, &
                              mnbor, nnbor, nbori)
!*******************************************************************************
!    Find neighbors among children of a given node at the previous level.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, chldp(*), idx(d,*), nlatt(d), ileaf, prnt, lvln
      logical, intent(in) :: per(d)
      integer, intent(inout) :: mnbor, nnbor
      integer, allocatable, intent(inout) :: nbori(:)

!     local variables
      integer :: jleaf, i, j
      logical :: nbor(d), malloc
!     ==========================================================================

!     loop through children
      do i = chldp(prnt)+1, chldp(prnt+1)
        jleaf = i - lvln

!       cycle if same node
        if (ileaf == jleaf) cycle

!       check if neighbor
        nbor = .false.
        do j = 1, d
          if (abs(idx(j,ileaf) - idx(j,jleaf)) <= 1) then
            nbor(j) = .true.
          endif
          if (per(j)) then
            if ((abs(idx(j,ileaf) - idx(j,jleaf) + nlatt(j)) <= 1) .or. &
                (abs(idx(j,ileaf) - idx(j,jleaf) - nlatt(j)) <= 1)) then
              nbor(j) = .true.
            endif
          endif
        enddo

!       add to neighbor list
        if (all(nbor)) then
          nnbor = nnbor + 1
          malloc = .false.
          if (mnbor == 0) then
            malloc = .true.
            mnbor = 1
          endif
          do while (nnbor > mnbor)
            malloc = .true.
            mnbor = 2*mnbor
          enddo
          if (malloc) call hypoct_malloc1i(nbori, mnbor, .true.)
          nbori(nnbor) = i
        endif
      enddo

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
      integer, intent(in) :: lvlp, xp(*), nodex(2,*), d, lvldiv
      real*8, intent(in) :: l(d)
      integer, intent(inout) :: n, leaf(n+1)
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

!     update leaf data
      leaf = xp(1:n+1) - xp(1)
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

!     delete empty leaves
      do i = 1, n
        if (leaf(i) == leaf(i+1)) n = n - 1
      enddo
      do i = 1, n
        if (leaf(i) == leaf(i+1)) then
          ctr(:,i:n-1) = ctr(:,i+1:n)
          leaf(i:n) = leaf(i+1:n+1)
        endif
      enddo

     end subroutine

    end module