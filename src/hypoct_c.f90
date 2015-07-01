!*******************************************************************************
!   Copyright (C) 2013-2015 Kenneth L. Ho
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
!   This module is part of the C interface to HYPOCT. See the header file
!   HYPOCT.H for more details.
!*******************************************************************************

     use iso_c_binding
     use hypoct
     implicit none

    contains

!*******************************************************************************
     subroutine hypoct_c_build(adap, elem, d, n, x, siz, occ, lvlmax, ext, &
                               lvlx, rootx, xi, xp, nodex) &
                bind(C)
!*******************************************************************************
!    C wrapper for HYPOCT_BUILD.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character(c_char), intent(in) :: adap, elem
      integer(c_int), intent(in) :: d, n, occ, lvlmax
      real(c_double), intent(in) :: x(*), siz(*), ext(*)
      integer(c_int), intent(out) :: xi(*)
      real(c_double), intent(out) :: rootx(*)
      type(c_ptr), intent(out) :: lvlx, xp, nodex

!     local variables
      integer, allocatable, target, save :: lvlx_(:,:), xp_(:), nodex_(:,:)
!     ==========================================================================

!     call Fortran routine
      call hypoct_build(adap, elem, d, n, x, siz, occ, lvlmax, ext, &
                        lvlx_, rootx, xi, xp_, nodex_)

!     set outputs
      lvlx  = c_loc(lvlx_)
      xp    = c_loc(xp_)
      nodex = c_loc(nodex_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_chld(lvlx, nodex, chldp) bind(C)
!*******************************************************************************
!    C wrapper for HYPOCT_CHLD.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: lvlx(*), nodex(*)
      type(c_ptr), intent(out) :: chldp

!     local variables
      integer, allocatable, target, save :: chldp_(:)
!     ==========================================================================

!     call Fortran routine
      call hypoct_chld(lvlx, nodex, chldp_)

!     set outputs
      chldp = c_loc(chldp_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_geom(d, lvlx, rootx, nodex, l, ctr) bind(C)
!*******************************************************************************
!    C wrapper for HYPOCT_GEOM.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: d, lvlx(*), nodex(*)
      real(c_double), intent(in) :: rootx(*)
      type(c_ptr), intent(out) :: l, ctr

!     local variables
      real*8, allocatable, target, save :: l_(:,:), ctr_(:,:)
!     ==========================================================================

!     call Fortran routine
      call hypoct_geom(d, lvlx, rootx, nodex, l_, ctr_)

!     set outputs
      l   = c_loc(l_)
      ctr = c_loc(ctr_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_ilst(lvlx, xp, nodex, chldp, nbori, nborp, &
                              ilsti, ilstp) &
                bind(C)
!*******************************************************************************
!    C wrapper for HYPOCT_ILST.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: lvlx(*), xp(*), nodex(*), chldp(*), &
                                    nbori(*), nborp(*)
      type(c_ptr), intent(out) :: ilsti, ilstp

!     local variables
      integer, allocatable, target, save :: ilsti_(:), ilstp_(:)
!     ==========================================================================

!     call Fortran routine
      call hypoct_ilst(lvlx, xp, nodex, chldp, nbori, nborp, &
                       ilsti_, ilstp_)

!     set outputs
      ilsti = c_loc(ilsti_)
      ilstp = c_loc(ilstp_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_nbor(elem, d, lvlx, xp, nodex, chldp, l, ctr, per, &
                              nbori, nborp) &
                bind(C)
!*******************************************************************************
!    C wrapper for HYPOCT_NBOR.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character(c_char), intent(in) :: elem
      integer(c_int), intent(in) :: d, lvlx(*), xp(*), nodex(*), chldp(*), &
                                    per(d)
      real(c_double), intent(in) :: l(*), ctr(*)
      type(c_ptr), intent(out) :: nbori, nborp

!     local variables
      integer, allocatable, target, save :: nbori_(:), nborp_(:)
      logical :: per_(d)
!     ==========================================================================

!     set inputs
      per_ = (per /= 0)

!     call Fortran routine
      call hypoct_nbor(elem, d, lvlx, xp, nodex, chldp, l, ctr, per_, &
                       nbori_, nborp_)

!     set outputs
      nbori = c_loc(nbori_)
      nborp = c_loc(nborp_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_search(elem, d, n, x, siz, mlvl, lvlx, nodex, chldp, &
                                l, ctr, trav) &
                bind(C)
!*******************************************************************************
!    C wrapper for HYPOCT_SEARCH.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character(c_char), intent(in) :: elem
      integer(c_int), intent(in) :: d, n, mlvl, lvlx(*), nodex(*), chldp(*)
      real(c_double), intent(in) :: x(*), siz(*), l(*), ctr(*)
      integer(c_int), intent(out) :: trav(n,0:mlvl)
!     ==========================================================================

!     call Fortran routine
      call hypoct_search(elem, d, n, x, siz, mlvl, lvlx, nodex, chldp, l, ctr, &
                         trav)

     end subroutine

    end module