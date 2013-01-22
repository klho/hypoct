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
!   HYPOCT contains procedures for constructing and manipulating point
!   hyperoctrees. Its primary purpose is to support fast tree-based algorithms
!   such as the fast multipole method. As such, it has capabilities for
!   generating neighbor and interaction lists, among others.
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
!     subdivision (at the scope of both nodes and levels) and pruning of empty
!     nodes.
!
!   - Support for periodicity.
!
!   HYPOCT is written mainly in a Fortran 77 style for compatibility, with
!   select Fortran 90 features for performance and accessibility.
!
!   ----------------------------------------------------------------------------
!   User-level procedures
!   ----------------------------------------------------------------------------
!   hypoct_build  - build hyperoctree
!   hypoct_buildx - build hyperoctree (expert)
!   hypoct_chld   - generate child data
!   hypoct_geom   - generate geometry data
!   hypoct_nbors  - find neighbors
!   hypoct_nborsx - find neighbors (expert)
!*******************************************************************************
     implicit none

    contains

!===============================================================================
!    User-level procedures
!===============================================================================

! LVLX: LVLP(0:NLVL+1)
!       NLVL LVLDIV(NLVL)
!
! ROOTX: CTR(D)
!        LRT(D)
!
! NODEX: NODEP(NNODE+1)
!        PRNT(NNODE) -1
!        CHLDID(NNODE) -1
!
!*******************************************************************************
     subroutine hypoct_build(d, n, x, occ, lvlx, rootx, xi, nodex)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, n, occ
      real*8, intent(in) :: x(d,n)
      integer, intent(out) :: xi(n)
      integer, allocatable, intent(out) :: lvlx(:,:), nodex(:,:)
      real*8, intent(out) :: rootx(2,d)

!     local variables
      character :: adap, intr
      integer :: lvlmax
      real*8 :: siz(n), ext(d)
!     ==========================================================================

!     set defaults
      adap = 'a'
      intr = 'p'
      siz = 0
      lvlmax = -1
      ext = 0

!     call main subroutine
      call hypoct_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
                         lvlx, rootx, xi, nodex)

     end subroutine

!*******************************************************************************
     subroutine hypoct_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
                              lvlx, rootx, xi, nodex)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: adap, intr
      integer, intent(in) :: d, n, occ, lvlmax
      real*8, intent(in) :: x(d,n), siz(n), ext(d)
      integer, intent(out) :: xi(n)
      integer, allocatable, intent(out) :: lvlx(:,:), nodex(:,:)
      real*8, intent(out) :: rootx(2,d)

!     local variables
      character(len=*), parameter :: srname = 'HYPOCT_BUILDX'
      integer :: mlvl, nlvl, mnode, nnode, nnode_, mleaf, nleaf, lvldiv, i
      integer, allocatable :: leaf(:)
      real*8, parameter :: divrat = 1 / sqrt(2d0)
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
      allocate(lvlx(2,0:mlvl+1), nodex(3,mnode+1), ctr(d,mleaf), &
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
        rootx(1,i) = 0.5*(xrng(1) + xrng(2))
        rootx(2,i) =      xrng(2) - xrng(1)
      enddo
      nodex(1,1:2) = (/ 0, n /)
      nodex(2,1) =  0
      nodex(3,1) = -1
      xi = (/ (i, i = 1, n) /)
      leaf(1:2) = nodex(1,1:2)
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
            l(i) = 0.5*l(i)
          endif
        enddo
        lrng = (/ minval(l), maxval(l) /)

!       hold points if appropriate
        call hypoct_hold(intr, siz, lrng(1), nleaf, leaf, div, &
                         xi(nodex(1,lvlx(1,nlvl)+1)+1), nodex(1,lvlx(1,nlvl)+1))

!       stop if none left
        if (leaf(nleaf+1) == 0) exit

!       divide leaves
        nnode_ = nnode
        do i = 1, nleaf
          if (div(i)) then
            call hypoct_subdiv(d, x, lvldiv, lvlx(1,nlvl)+i, ctr(1,i), &
                               leaf(i+1)-leaf(i), &
                               xi(nodex(1,lvlx(1,nlvl+1)+1)+leaf(i)+1), mnode, &
                               nnode, nodex)
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
        call hypoct_update(lvlx(1,nlvl), nodex(1,lvlx(1,nlvl+1)+1), d, l, &
                           lvldiv, nleaf, leaf, ctr)

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
        call hypoct_malloc2i(nodex, 3, nnode+1, .true.)
      endif
      nodex(2:3,nnode+1) = -1

     end subroutine

!*******************************************************************************
     subroutine hypoct_chld(lvlx, nodex, chldx)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: lvlx(2,0:*), nodex(3,*)
      integer, allocatable, intent(out) :: chldx(:)

!     local variables
      integer :: nlvl, i, j
!     ==========================================================================

!     initialize
      nlvl = lvlx(2,0)
      allocate(chldx(lvlx(1,nlvl+1)+1))
      chldx = 0

!     count number of children for each node
      do i = 1, nlvl
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          chldx(nodex(2,j)+1) = chldx(nodex(2,j)+1) + 1
        enddo
      enddo

!     do cumulative sum
      chldx(1) = 1
      do i = 1, lvlx(1,nlvl+1)
        chldx(i+1) = chldx(i) + chldx(i+1)
      enddo

     end subroutine

!*******************************************************************************
     subroutine hypoct_geom(d, lvlx, rootx, nodex, l, ctr)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, lvlx(2,0:*), nodex(3,*)
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

!     propagate geometry information
      do i = 1, nlvl
        l(:,i) = l(:,i-1)
        do j = 1, d
          if (ibits(lvlx(2,i+1), j-1, 1) == 1) then
            div(j) = .true.
            l(j,i) = 0.5*l(j,i)
          endif
        enddo
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          ctr(:,j) = ctr(:,nodex(2,j))
          do k = 1, d
            if (div(k)) then
              if (ibits(nodex(3,j), k-1, 1) == 0) then
                ctr(k,j) = ctr(k,j) - 0.5*l(k,i)
              else
                ctr(k,j) = ctr(k,j) + 0.5*l(k,i)
              endif
            endif
          enddo
        enddo
      enddo

     end subroutine

!*******************************************************************************
     subroutine hypoct_nbors(d, lvlx, nodex, nborp, nbori)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, lvlx(2,0:*), nodex(3,*)
      integer, allocatable, intent(out) :: nborp(:), nbori(:)

!     local variables
      integer, allocatable :: chldx(:)
      logical :: per(d)
!     ==========================================================================

!     set defaults
      call hypoct_chld(lvlx, nodex, chldx)
      per = .false.

!     call main subroutine
      call hypoct_nborsx(d, lvlx, nodex, chldx, per, nborp, nbori)

     end subroutine

!*******************************************************************************
     subroutine hypoct_nborsx(d, lvlx, nodex, chldx, per, nborp, nbori)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, lvlx(2,0:*), nodex(3,*), chldx(*)
      logical, intent(in) :: per(d)
      integer, allocatable, intent(out) :: nborp(:), nbori(:)

!     local variables
      integer :: nlvl, nleaf, mnbor, nnbor, nlatt(d), ileaf, prnt, i, j, k
      integer, allocatable :: idx(:,:), idx_(:,:)
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
        mnbor = nleaf*(nleaf - 1)
        allocate(nbori(mnbor))
        do i = lvlx(1,1)+1, lvlx(1,2)
          nborp(i+1) = nborp(i) + nleaf - 1
        enddo
        nnbor = 0
        do i = 1, nleaf
          do j = 1, d
            idx(j,i) = ibits(nodex(3,lvlx(1,1)+i), j-1, 1)
          enddo
          do j = 1, nleaf
            if (i /= j) then
              nnbor = nnbor + 1
              nbori(nnbor) = lvlx(1,1) + j
            endif
          enddo
        enddo
        if (any(per)) then
          do i = 1, d
            nlatt(i) = ibits(lvlx(2,2), i-1, 1) + 1
          enddo
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
          idx(:,ileaf) = idx_(:,nodex(2,j)-lvlx(1,i-1))
          do k = 1, d
            if (ibits(lvlx(2,i+1), k-1, 1) == 1) then
              idx(k,ileaf) = 2*idx(k,ileaf)
              if (ibits(nodex(3,j), k-1, 1) == 1) then
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

!       look through children of parent's neighbors
        do j = lvlx(1,i)+1, lvlx(1,i+1)
          ileaf = j - lvlx(1,i)
          prnt = nodex(2,j)
          call hypoct_chldnbor(d, chldx, per, idx, nlatt, ileaf, prnt, &
                               lvlx(1,i), mnbor, nnbor, nbori)
          do k = nborp(prnt)+1, nborp(prnt+1)
            call hypoct_chldnbor(d, chldx, per, idx, nlatt, ileaf, nbori(k), &
                                 lvlx(1,i), mnbor, nnbor, nbori)
          enddo
          nborp(j+1) = nnbor
        enddo
      enddo

!     resize output arrays
      if (nnbor < mnbor) then
        call hypoct_malloc1i(nbori, nnbor, .true.)
      endif

     end subroutine

!===============================================================================
!    Internal procedures
!===============================================================================

!*******************************************************************************
     function hypoct_big(intr, siz, lmin) result(big)
!*******************************************************************************
!*******************************************************************************
      implicit none

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

      if (intr == 'p') then
        big = .false.
        return
      endif

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
!*******************************************************************************
      implicit none

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
     subroutine hypoct_chldnbor(d, chldx, per, idx, nlatt, ileaf, prnt, lvln, &
                                mnbor, nnbor, nbori)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, chldx(*), idx(d,*), nlatt(d), ileaf, prnt, lvln
      logical, intent(in) :: per(d)
      integer, intent(inout) :: mnbor, nnbor
      integer, allocatable, intent(inout) :: nbori(:)

!     local variables
      integer :: jleaf, i, j
      logical :: nbor(d), malloc
!     ==========================================================================

!     loop through children
      do i = chldx(prnt)+1, chldx(prnt+1)
        jleaf = i - lvln

!       cycle if same node
        if (ileaf ==  jleaf) cycle

!       check if neighbor
        nbor = .false.
        do j = 1, d
          if (abs(idx(j,ileaf) - idx(j,jleaf)) < 2) then
            nbor(j) = .true.
          endif
          if (per(j)) then
            if ((abs(idx(j,ileaf) - idx(j,jleaf) + nlatt(j)) < 2) .or. &
                (abs(idx(j,ileaf) - idx(j,jleaf) - nlatt(j)) < 2)) then
              nbor(j) = .true.
            endif
          endif
        enddo

!       add to neighbor list
        if (all(nbor)) then
          nnbor = nnbor + 1
          malloc = .false.
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
     subroutine hypoct_errmsg(msg)
!*******************************************************************************
!*******************************************************************************
      implicit none

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
!*******************************************************************************
      implicit none

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
     subroutine hypoct_hold(intr, siz, lmin, nleaf, leaf, div, xi, nodex)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: intr
      real*8, intent(in) :: siz(*), lmin
      integer, intent(inout) :: nleaf, leaf(nleaf+1), xi(leaf(nleaf+1)), &
                                nodex(3,nleaf+1)
      logical, intent(inout) :: div(nleaf)

!     local variables
      integer :: ptr(2,nleaf), xi_(leaf(nleaf+1)), i, j, k
      logical :: hold(leaf(nleaf+1))
!     ==========================================================================

!     count points to hold
      nodex(1,2:nleaf+1) = 0
      do i = 1, nleaf
        if (.not. div(i)) then
          hold(leaf(i)+1:leaf(i+1)) = .true.
        else
          do j = leaf(i)+1, leaf(i+1)
            hold(j) = hypoct_big(intr, siz(xi(j)), lmin)
          enddo
        endif
        do j = leaf(i)+1, leaf(i+1)
          if (hold(j)) nodex(1,i+1) = nodex(1,i+1) + 1
        enddo
      enddo
      do i = 1, nleaf
        nodex(1,i+1) = nodex(1,i) + nodex(1,i+1)
      enddo

!     sort points by hold status
      ptr(1,:) = nodex(1,1:nleaf) - nodex(1,1)
      ptr(2,:) = leaf(1:nleaf) - nodex(1,1:nleaf) + nodex(1,nleaf+1)
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
      leaf = leaf - nodex(1,:)
      leaf = leaf - leaf(1)

     end subroutine

!*******************************************************************************
     subroutine hypoct_malloc1i(a, ub, copy)
!*******************************************************************************
!*******************************************************************************
      implicit none

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
!*******************************************************************************
      implicit none

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
!*******************************************************************************
      implicit none

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
!*******************************************************************************
      implicit none

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
     subroutine hypoct_subdiv(d, x, lvldiv, prnt, ctr, n, xi, mnode, nnode, &
                              nodex)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, lvldiv, prnt, n
      real*8, intent(in) :: x(d,*), ctr(d)
      integer, intent(inout) :: xi(n), mnode, nnode
      integer, allocatable, intent(inout) :: nodex(:,:)

!     local variables
      integer :: nchld, id(n), xi_(n), i, j, k
      integer, allocatable :: ptr(:)
      logical :: new,  malloc
!     ==========================================================================

!     find new leaves based on child information
      nchld = 0
      do i = 1, n
        id(i) = hypoct_chldid(d, x(1,xi(i)), ctr, lvldiv)
        new = .true.
        do j = 1, nchld
          if (nodex(3,nnode+j) >  id(i)) exit
          if (nodex(3,nnode+j) == id(i)) then
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
          if (malloc) call hypoct_malloc2i(nodex, 3, mnode+1, .true.)
          do k = nchld-1, j, -1
            nodex(1,nnode+k+2) = nodex(1,nnode+k+1)
            nodex(2,nnode+k+1) = nodex(2,nnode+k)
            nodex(3,nnode+k+1) = nodex(3,nnode+k)
          enddo
          nodex(1,nnode+j+1) = 1
          nodex(2:3,nnode+j) = (/ prnt, id(i) /)
        else
          nodex(1,nnode+j+1) = nodex(1,nnode+j+1) + 1
        endif
      enddo
      do i = 1, nchld
        nodex(1,nnode+i+1) = nodex(1,nnode+i) + nodex(1,nnode+i+1)
      enddo

!     sort points according to new leaves
      allocate(ptr(nchld))
      ptr = nodex(1,nnode+1:nnode+nchld) - nodex(1,nnode+1)
      xi_ = xi
      do i = 1, n
        do j = 1, nchld
          if (nodex(3,nnode+j) == id(i)) exit
        enddo
        ptr(j) = ptr(j) + 1
        xi(ptr(j)) = xi_(i)
      enddo

!     update number of nodes
      nnode = nnode + nchld

     end subroutine

!*******************************************************************************
     subroutine hypoct_update(lvlp, nodex, d, l, lvldiv, n, leaf, ctr)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: lvlp, nodex(3,*), d, lvldiv
      real*8, intent(in) :: l(d)
      integer, intent(inout) :: n, leaf(n+1)
      real*8, intent(inout) :: ctr(d,*)

!     local variables
      integer :: id, i, j
      real*8 :: ctr_(d,n)
!     ==========================================================================

!     copy centers of parents
      do i = 1, n
        ctr_(:,i) = ctr(:,nodex(2,i)-lvlp)
      enddo
      ctr(:,1:n) = ctr_

!     update leaf data
      leaf = nodex(1,1:n+1) - nodex(1,1)
      do i = 1, n
        id = nodex(3,i)
        do j = 1, d
          if (ibits(lvldiv, j-1, 1) == 1) then
            if (ibits(id, j-1, 1) == 0) then
              ctr(j,i) = ctr(j,i) - 0.5*l(j)
            else
              ctr(j,i) = ctr(j,i) + 0.5*l(j)
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