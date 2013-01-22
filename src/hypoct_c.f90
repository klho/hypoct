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
!*******************************************************************************

     use iso_c_binding
     use hypoct
     implicit none

    contains

!*******************************************************************************
     subroutine hypoct_c_build(d, n, x, occ, lvlx, rootx, xi, nodex) &
                bind(C, name='hypoct_build')
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      integer(c_int), intent(in) :: d, n, occ
      real(c_double), intent(in) :: x(d,n)
      integer(c_int), intent(out) :: xi(n)
      real(c_double), intent(out) :: rootx(2,d)
      type(c_ptr), intent(out) :: lvlx, nodex

!     local variables
      integer(c_int), allocatable, target, save :: lvlx_(:,:), nodex_(:,:)
!     ==========================================================================

      call hypoct_build(d, n, x, occ, lvlx_, rootx, xi, nodex_)

      lvlx  = c_loc(lvlx_)
      nodex = c_loc(nodex_)

     end subroutine

!*******************************************************************************
     subroutine hypoct_c_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
                                lvlx, rootx, xi, nodex) &
                bind(C, name='hypoct_buildx')
!*******************************************************************************
!*******************************************************************************
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     arguments
      character(c_char), intent(in) :: adap, intr
      integer(c_int), intent(in) :: d, n, occ, lvlmax
      real(c_double), intent(in) :: x(d,n), siz(n), ext(d)
      integer(c_int), intent(out) :: xi(n)
      real(c_double), intent(out) :: rootx(2,d)
      type(c_ptr), intent(out) :: lvlx, nodex

!     local variables
      integer(c_int), allocatable, target, save :: lvlx_(:,:), nodex_(:,:)
!     ==========================================================================

      call hypoct_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
                         lvlx_, rootx, xi, nodex_)

      lvlx  = c_loc(lvlx_)
      nodex = c_loc(nodex_)

     end subroutine

    end module