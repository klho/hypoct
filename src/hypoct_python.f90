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
    module hypoct_python
!*******************************************************************************
!*******************************************************************************

     use hypoct
     implicit none
     integer, allocatable :: lvlx(:,:), nodex(:,:), chldx(:), nborp(:), nbori(:)
     real*8, allocatable :: l(:,:), ctr(:,:)

    contains

!*******************************************************************************
     subroutine hypoct_python_buildx(adap, intr, d, n, x, siz, occ, lvlmax, &
                                     ext, rootx, xi)
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
      real*8, intent(out) :: rootx(2,d)
!     ==========================================================================

      call hypoct_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, lvlx, &
                         rootx, xi, nodex)

     end subroutine

!*******************************************************************************
     subroutine hypoct_python_chld(lvlx, nodex)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: lvlx(2,0:*), nodex(3,*)
!     ==========================================================================

      call hypoct_chld(lvlx, nodex, chldx)

     end subroutine

!*******************************************************************************
     subroutine hypoct_python_geom(d, lvlx, rootx, nodex)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, lvlx(2,0:*), nodex(3,*)
      real*8, intent(in) :: rootx(2,d)
!     ==========================================================================

      call hypoct_geom(d, lvlx, rootx, nodex, l, ctr)

     end subroutine

!*******************************************************************************
     subroutine hypoct_python_nborsx(d, lvlx, nodex, chldx, per)
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer, intent(in) :: d, lvlx(2,0:*), nodex(3,*), chldx(*)
      logical, intent(in) :: per(d)
!     ==========================================================================

      call hypoct_nborsx(d, lvlx, nodex, chldx, per, nborp, nbori)

     end subroutine

    end module