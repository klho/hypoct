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
!   This file contains part of the C interface to HYPOCT.
!*******************************************************************************

     use iso_c_binding
     use hypoct
     implicit none

    contains

!*******************************************************************************
     subroutine hypoct_c_build(d, n, x, occ, nlvl, nnode, lvlx, rootx, xi, &
                               nodex) &
                bind(C, name='hypoct_build')
!*******************************************************************************
!    C wrapper for HYPOCT_BUILD.
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: d, n, occ
      real(c_double), intent(in) :: x(d,n)
      integer(c_int), intent(out) :: xi(n), nlvl, nnode
      real(c_double), intent(out) :: rootx(2,d)
      type(c_ptr), intent(out) :: lvlx, nodex

!     local variables
      integer, allocatable, target, save :: lvlx_(:,:), nodex_(:,:)
!     ==========================================================================

!     call Fortran routine
      call hypoct_build(d, n, x, occ, lvlx_, rootx, xi, nodex_)

!     set outputs
      nlvl  = lvlx_(2,0)
      nnode = lvlx_(1,nlvl+1)
      lvlx  = c_loc(lvlx_)
      nodex = c_loc(nodex_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
                                nlvl, nnode, lvlx, rootx, xi, nodex) &
                bind(C, name='hypoct_buildx')
!*******************************************************************************
!    C wrapper for HYPOCT_BUILDX.
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character(c_char), intent(in) :: adap, intr
      integer(c_int), intent(in) :: d, n, occ, lvlmax
      real(c_double), intent(in) :: x(d,n), siz(n), ext(d)
      integer(c_int), intent(out) :: xi(n), nlvl, nnode
      real(c_double), intent(out) :: rootx(2,d)
      type(c_ptr), intent(out) :: lvlx, nodex

!     local variables
      integer, allocatable, target, save :: lvlx_(:,:), nodex_(:,:)
!     ==========================================================================

!     call Fortran routine
      call hypoct_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
                         lvlx_, rootx, xi, nodex_)

!     set outputs
      nlvl  = lvlx_(2,0)
      nnode = lvlx_(1,nlvl+1)
      lvlx  = c_loc(lvlx_)
      nodex = c_loc(nodex_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_chld(nlvl, nnode, lvlx, nodex, chldp) &
                bind(C, name="hypoct_chld")
!*******************************************************************************
!    C wrapper for HYPOCT_CHLD
!*******************************************************************************
      implicit none

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
      call c_f_pointer(nodex, nodex_, [3, nnode+1])

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
      implicit none

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
      call c_f_pointer(nodex, nodex_, [3, nnode+1])

!     call Fortran routine
      call hypoct_geom(d, lvlx_, rootx, nodex_, l_, ctr_)

!     set outputs
      l   = c_loc(l_)
      ctr = c_loc(ctr_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_ilst(nlvl, nnode, lvlx, nodex, chldp, nnbor, nborp, &
                              nbori, nilst, ilstp, ilsti) &
                bind(C, name="hypoct_ilst")
!*******************************************************************************
!    C wrapper for HYPOCT_ILST.
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: nlvl, nnode, nnbor
      type(c_ptr), intent(in) :: lvlx, nodex, chldp, nborp, nbori
      integer(c_int), intent(out) :: nilst
      type(c_ptr), intent(out) :: ilstp, ilsti

!     local variables
      integer, pointer :: lvlx_(:,:), nodex_(:,:), chldp_(:), nborp_(:), &
                          nbori_(:)
      integer, allocatable, target, save :: ilstp_(:), ilsti_(:)
!     ==========================================================================

!     set inputs
      call c_f_pointer(lvlx,  lvlx_,  [2, nlvl+2])
      call c_f_pointer(nodex, nodex_, [3, nnode+1])
      call c_f_pointer(chldp, chldp_, [nnode+1])
      call c_f_pointer(nborp, nborp_, [nnode+1])
      call c_f_pointer(nbori, nbori_, [nnbor])

!     call Fortran routine
      call hypoct_ilst(lvlx_, nodex_, chldp_, nborp_, nbori_, ilstp_, ilsti_)

!     set outputs
      nilst = ilstp_(nnode+1)
      ilstp = c_loc(ilstp_)
      ilsti = c_loc(ilsti_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_nbor(d, nlvl, nnode, lvlx, nodex, &
                              nnbor, nborp, nbori) &
                bind(C, name="hypoct_nbor")
!*******************************************************************************
!    C wrapper for HYPOCT_NBOR.
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: d, nlvl, nnode
      type(c_ptr), intent(in) :: lvlx, nodex
      integer(c_int), intent(out) :: nnbor
      type(c_ptr), intent(out) :: nborp, nbori

!     local variables
      integer, pointer :: lvlx_(:,:), nodex_(:,:)
      integer, allocatable, target, save :: nborp_(:), nbori_(:)
!     ==========================================================================

!     set inputs
      call c_f_pointer(lvlx,  lvlx_,  [2, nlvl+2])
      call c_f_pointer(nodex, nodex_, [3, nnode+1])

!     call Fortran routine
      call hypoct_nbor(d, lvlx_, nodex_, nborp_, nbori_)

!     set outputs
      nnbor = nborp_(nnode+1)
      nborp = c_loc(nborp_)
      nbori = c_loc(nbori_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_nborx(d, nlvl, nnode, lvlx, nodex, chldp, per, &
                               nnbor, nborp, nbori) &
                bind(C, name="hypoct_nborx")
!*******************************************************************************
!    C wrapper for HYPOCT_NBORX.
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: d, nlvl, nnode, per(d)
      type(c_ptr), intent(in) :: lvlx, nodex, chldp
      integer(c_int), intent(out) :: nnbor
      type(c_ptr), intent(out) :: nborp, nbori

!     local variables
      integer, pointer :: lvlx_(:,:), nodex_(:,:), chldp_(:)
      integer, allocatable, target, save :: nborp_(:), nbori_(:)
      logical :: per_(d)
!     ==========================================================================

!     set inputs
      call c_f_pointer(lvlx,  lvlx_,  [2, nlvl+2])
      call c_f_pointer(nodex, nodex_, [3, nnode+1])
      call c_f_pointer(chldp, chldp_, [nnode+1])
      per_ = (per /= 0)

!     call Fortran routine
      call hypoct_nborx(d, lvlx_, nodex_, chldp_, per_, nborp_, nbori_)

!     set outputs
      nnbor = nborp_(nnode+1)
      nborp = c_loc(nborp_)
      nbori = c_loc(nbori_)

     end subroutine

    end module