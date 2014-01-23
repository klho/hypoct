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
    module hypoct_python
!*******************************************************************************
!   HYPOCT_PYTHON - Python wrapper for HYPOCT
!
!   This module is part of the Python interface to HYPOCT. See the Python module
!   for more details.
!*******************************************************************************

     use hypoct
     implicit none

!    allocatable arrays
     integer, allocatable :: lvlx(:,:), xp(:), nodex(:,:), chldp(:), &
                             nbori(:), nborp(:), ilsti(:), ilstp(:)
     real*8, allocatable :: l(:,:), ctr(:,:)

    contains

!*******************************************************************************
     subroutine hypoct_python_build(adap, elem, d, n, x, siz, occ, lvlmax, &
                                    ext, rootx, xi)
!*******************************************************************************
!    Python wrapper for HYPOCT_BUILD.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: adap, elem
      integer, intent(in) :: d, n, occ, lvlmax
      real*8, intent(in) :: x(d,n), siz(n), ext(d)
      integer, intent(out) :: xi(n)
      real*8, intent(out) :: rootx(2,d)
!     ==========================================================================

      call hypoct_build(adap, elem, d, n, x, siz, occ, lvlmax, ext, &
                        lvlx, rootx, xi, xp, nodex)

     end subroutine

!*******************************************************************************
     subroutine hypoct_python_chld(lvlx, nodex)
!*******************************************************************************
!    Python wrapper for HYPOCT_CHLD.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: lvlx(2,0:*), nodex(2,*)
!     ==========================================================================

      call hypoct_chld(lvlx, nodex, chldp)

     end subroutine

!*******************************************************************************
     subroutine hypoct_python_geom(d, lvlx, rootx, nodex)
!*******************************************************************************
!    Python wrapper for HYPOCT_GEOM.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, lvlx(2,0:*), nodex(2,*)
      real*8, intent(in) :: rootx(2,d)
!     ==========================================================================

      call hypoct_geom(d, lvlx, rootx, nodex, l, ctr)

     end subroutine

!*******************************************************************************
     subroutine hypoct_python_ilst(lvlx, xp, nodex, chldp, nbori, nborp)
!*******************************************************************************
!    Python wrapper for HYPOCT_ILST.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: lvlx(2,0:*), xp(*), nodex(2,*), chldp(*), &
                             nbori(*), nborp(*)
!     ==========================================================================

      call hypoct_ilst(lvlx, xp, nodex, chldp, nbori, nborp, ilsti, ilstp)

     end subroutine

!*******************************************************************************
     subroutine hypoct_python_nbor(elem, d, lvlx, xp, nodex, chldp, l, ctr, per)
!*******************************************************************************
!    Python wrapper for HYPOCT_NBOR.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: elem
      integer, intent(in) :: d, lvlx(2,0:*), xp(*), nodex(2,*), chldp(*)
      real*8, intent(in) :: l(d,*), ctr(d,*)
      logical, intent(in) :: per(d)
!     ==========================================================================

      call hypoct_nbor(elem, d, lvlx, xp, nodex, chldp, l, ctr, per, &
                       nbori, nborp)

     end subroutine

!*******************************************************************************
     subroutine hypoct_python_search(elem, d, n, x, siz, mlvl, lvlx, nodex, &
                                     chldp, l, ctr, trav)
!*******************************************************************************
!    Python wrapper for HYPOCT_SEARCH.
!*******************************************************************************

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character, intent(in) :: elem
      integer, intent(in) :: d, n, mlvl, lvlx(2,0:*), nodex(2,*), chldp(*)
      real*8, intent(in) :: x(d,n), siz(n), l(d,0:*), ctr(d,*)
      integer, intent(out) :: trav(n,0:mlvl)
!     ==========================================================================

      call hypoct_search(elem, d, n, x, siz, mlvl, lvlx, nodex, chldp, l, ctr, &
                         trav)

     end subroutine

    end module