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

    program hypoct_driver

      use hypoct
      implicit none

!     build variables
      character :: adap, intr
      integer :: d, n, occ, lvlmax
      integer, allocatable :: lvlx(:,:), xi(:), nodex(:,:)

      real*8, allocatable :: x(:,:), siz(:), ext(:), rootx(:,:)

!     geometry variables
      real*8, allocatable :: l(:,:), ctr(:,:)

!     child variables
      integer, allocatable :: chldp(:)

!     neighbor variables
      integer, allocatable :: nborp(:), nbori(:)
      logical, allocatable :: per(:)

!     interaction list variables
      integer, allocatable :: ilstp(:), ilsti(:)

!     local variables
      integer :: i
!       real*4 :: t, t0
      real*8, parameter :: pi = 4*atan(1d0)
      real*8 :: theta

      adap = 'a'
      intr = 'p'
      d = 2
      n = 100
      allocate(x(d,n), siz(n), ext(d), rootx(2,d), xi(n))
      do i = 1, n
        theta = 2*pi*(i - 1) / n
        x(1,i) = cos(theta)
        x(2,i) = sin(theta)
      enddo
!       call random_number(x)
      siz = 0
      occ = 1
      lvlmax = -1
      ext = 0

!       call cpu_time(t0)
      call hypoct_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, lvlx, &
                         rootx, xi, nodex)
!       call cpu_time(t)
!       print *, t - t0
!       stop

!       print '(i8)', xi
!       print *
!       print '(2(i8))', lvlx
!       print *
!       print '(3(i8))', nodex
!       print *

!       call cpu_time(t0)
      call hypoct_geom(d, lvlx, rootx, nodex, l, ctr)
!       call cpu_time(t)
!       print *, t - t0

!       print *
!       print '(2(e12.4))', l
!       print *
!       print '(2(e12.4))', ctr

      call hypoct_chld(lvlx, nodex, chldp)

!       print '(i8)', chldp

!       call hypoct_nbor(d, lvlx, nodex, nborp, nbori)
      allocate(per(d))
      per = .false.
      call hypoct_nborx(d, lvlx, nodex, chldp, per, nborp, nbori)

!       print *
!       print '(i8)', nborp
!       print *
!       print '(i8)', nbori

      call hypoct_ilst(lvlx, nodex, chldp, nborp, nbori, ilstp, ilsti)

!       print *
!       print '(i8)', ilstp
!       print *
      print '(i8)', ilsti

    end program