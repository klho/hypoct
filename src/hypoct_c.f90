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
    module hypoct_c
!*******************************************************************************
!   HYPOCT_C - C wrapper for HYPOCT
!
!   This module is part of the C interface to HYPOCT.
!*******************************************************************************

     use iso_c_binding
     use hypoct
     implicit none

    contains

!*******************************************************************************
     subroutine hypoct_c_build(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
                               nlvl, nnode, lvlx, rootx, xi, xp, nodex) &
                bind(C, name='hypoct_build')
!*******************************************************************************
!    C wrapper for HYPOCT_BUILD.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character(c_char), intent(in) :: adap, intr
      integer(c_int), intent(in) :: d, n, occ, lvlmax
      real(c_double), intent(in) :: x(d,n), siz(n), ext(d)
      integer(c_int), intent(out) :: xi(n), nlvl, nnode
      real(c_double), intent(out) :: rootx(2,d)
      type(c_ptr), intent(out) :: lvlx, xp, nodex

!     local variables
      integer, allocatable, target, save :: lvlx_(:,:), xp_(:), nodex_(:,:)
!     ==========================================================================

!     call Fortran routine
      call hypoct_build(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
                        lvlx_, rootx, xi, xp_, nodex_)

!     set outputs
      nlvl  = lvlx_(2,0)
      nnode = lvlx_(1,nlvl+1)
      lvlx  = c_loc(lvlx_)
      xp    = c_loc(xp_)
      nodex = c_loc(nodex_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_chld(nlvl, nnode, lvlx, nodex, chldp) &
                bind(C, name="hypoct_chld")
!*******************************************************************************
!    C wrapper for HYPOCT_CHLD.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: nlvl, nnode
      type(c_ptr), intent(in) :: lvlx, nodex
      type(c_ptr), intent(out) :: chldp

!     local variables
      integer, pointer :: lvlx_(:,:), nodex_(:,:)
      integer, allocatable, target, save :: chldp_(:)
!     ==========================================================================

!     set inputs
      call c_f_pointer(lvlx,  lvlx_,  [2, nlvl+2])
      call c_f_pointer(nodex, nodex_, [2, nnode])

!     call Fortran routine
      call hypoct_chld(lvlx_, nodex_, chldp_)

!     set outputs
      chldp = c_loc(chldp_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_geom(d, nlvl, nnode, lvlx, rootx, nodex, l, ctr) &
                bind(C, name="hypoct_geom")
!*******************************************************************************
!    C wrapper for HYPOCT_GEOM.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: d, nlvl, nnode
      real(c_double), intent(in) :: rootx(2,d)
      type(c_ptr), intent(in) :: lvlx, nodex
      type(c_ptr), intent(out) :: l, ctr

!     local variables
      integer, pointer :: lvlx_(:,:), nodex_(:,:)
      real*8, allocatable, target, save :: l_(:,:), ctr_(:,:)
!     ==========================================================================

!     set inputs
      call c_f_pointer(lvlx,  lvlx_,  [2, nlvl+2])
      call c_f_pointer(nodex, nodex_, [2, nnode])

!     call Fortran routine
      call hypoct_geom(d, lvlx_, rootx, nodex_, l_, ctr_)

!     set outputs
      l   = c_loc(l_)
      ctr = c_loc(ctr_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_ilst(nlvl, nnode, lvlx, nodex, chldp, nnbor, nbori, &
                              nborp, nilst, ilsti, ilstp) &
                bind(C, name="hypoct_ilst")
!*******************************************************************************
!    C wrapper for HYPOCT_ILST.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: nlvl, nnode, nnbor
      type(c_ptr), intent(in) :: lvlx, nodex, chldp, nbori, nborp
      integer(c_int), intent(out) :: nilst
      type(c_ptr), intent(out) :: ilsti, ilstp

!     local variables
      integer, pointer :: lvlx_(:,:), nodex_(:,:), chldp_(:), nbori_(:), &
                          nborp_(:)
      integer, allocatable, target, save :: ilsti_(:), ilstp_(:)
!     ==========================================================================

!     set inputs
      call c_f_pointer(lvlx,  lvlx_,  [2, nlvl+2])
      call c_f_pointer(nodex, nodex_, [2, nnode])
      call c_f_pointer(chldp, chldp_, [nnode+1])
      call c_f_pointer(nbori, nbori_, [nnbor])
      call c_f_pointer(nborp, nborp_, [nnode+1])

!     call Fortran routine
      call hypoct_ilst(lvlx_, nodex_, chldp_, nbori_, nborp_, ilsti_, ilstp_)

!     set outputs
      nilst = ilstp_(nnode+1)
      ilsti = c_loc(ilsti_)
      ilstp = c_loc(ilstp_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_nbor(d, nlvl, nnode, lvlx, xp, nodex, chldp, per, &
                              nnbor, nbori, nborp) &
                bind(C, name="hypoct_nbor")
!*******************************************************************************
!    C wrapper for HYPOCT_NBOR.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: d, nlvl, nnode, per(d)
      type(c_ptr), intent(in) :: lvlx, xp, nodex, chldp
      integer(c_int), intent(out) :: nnbor
      type(c_ptr), intent(out) :: nbori, nborp

!     local variables
      integer, pointer :: lvlx_(:,:), xp_(:), nodex_(:,:), chldp_(:)
      integer, allocatable, target, save :: nbori_(:), nborp_(:)
      logical :: per_(d)
!     ==========================================================================

!     set inputs
      call c_f_pointer(lvlx,  lvlx_,  [2, nlvl+2])
      call c_f_pointer(xp,    xp_,    [nnode+1])
      call c_f_pointer(nodex, nodex_, [2, nnode])
      call c_f_pointer(chldp, chldp_, [nnode+1])
      per_ = (per /= 0)

!     call Fortran routine
      call hypoct_nbor(d, lvlx_, xp_, nodex_, chldp_, per_, nbori_, nborp_)

!     set outputs
      nnbor = nborp_(nnode+1)
      nbori = c_loc(nbori_)
      nborp = c_loc(nborp_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_search(d, n, x, mlvl, nlvl, nnode, lvlx, rootx, &
                                nodex, chldp, ctr, trav) &
                bind(C, name="hypoct_search")
!*******************************************************************************
!    C wrapper for HYPOCT_SEARCH.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: d, n, mlvl, nlvl, nnode
      real(c_double), intent(in) :: x(d,n), rootx(2,d)
      type(c_ptr), intent(in) :: lvlx, nodex, chldp, ctr
      integer(c_int), intent(out) :: trav(n,0:mlvl)

!     local variables
      integer, pointer :: lvlx_(:,:), nodex_(:,:), chldp_(:)
      real*8, pointer :: ctr_(:,:)
!     ==========================================================================

!     set inputs
      call c_f_pointer(lvlx,  lvlx_,  [2, nlvl+2])
      call c_f_pointer(nodex, nodex_, [2, nnode])
      call c_f_pointer(chldp, chldp_, [nnode+1])
      call c_f_pointer(ctr,   ctr_,   [2, nnode])

!     call Fortran routine
      call hypoct_search(d, n, x, mlvl, lvlx_, rootx, nodex_, chldp_, ctr_, &
                         trav)

     end subroutine

    end module