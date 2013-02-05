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
    program hypoct_driver
!*******************************************************************************
!   Build quadtree on uniformly spaced points on the unit circle.
!*******************************************************************************
      use hypoct
      implicit none

!     ==========================================================================
!     variable declarations
!     --------------------------------------------------------------------------
!     build variables
      character :: adap = 'a', intr = 'p'
      integer :: d = 2, n = 2**20, occ = 20, lvlmax = -1
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
      integer :: nlvl, nnode, i
      real*4 :: t, t0
      real*8, parameter :: pi = 4*atan(1d0), i2mb = 4d-6, d2mb = 8d-6
      real*8 :: theta, mb
!     ==========================================================================

!     set inputs
      allocate(x(d,n), siz(n), ext(d), rootx(2,d), xi(n))
      do i = 1, n
        theta = 2*pi*(i - 1) / n
        x(1,i) = cos(theta)
        x(2,i) = sin(theta)
      enddo
      siz = 0
      ext = 0

!     print input summary
      print 10, n

10    format(' Number of points:                           ', i8, /, &
             ' ----------------------------------------------------')

!     build tree
      print '(xa,$)', 'Building tree...             '
      call cpu_time(t0)
      call hypoct_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, &
                         lvlx, rootx, xi, nodex)
      call cpu_time(t)
      mb = i2mb*(size(lvlx) + size(xi) + size(nodex)) + d2mb*size(rootx)
      print 20, t - t0, mb

20    format(e10.4, ' (s) / ', f6.2, ' (MB)')

!     generate child data
      print '(xa,$)', 'Generating child data...     '
      call cpu_time(t0)
      call hypoct_chld(lvlx, nodex, chldp)
      call cpu_time(t)
      mb = i2mb*size(chldp)
      print 20, t - t0, mb

!     generate geometry data
      print '(xa,$)', 'Generating geometry data...  '
      call cpu_time(t0)
      call hypoct_geom(d, lvlx, rootx, nodex, l, ctr)
      call cpu_time(t)
      mb = d2mb*(size(l) + size(ctr))
      print 20, t - t0, mb

!     find neighbors
      print '(xa,$)', 'Finding neighbors...         '
      allocate(per(d))
      per = .false.
      call cpu_time(t0)
      call hypoct_nborx(d, lvlx, nodex, chldp, per, nborp, nbori)
      call cpu_time(t)
      mb = i2mb*(size(nborp) + size(nbori))
      print 20, t - t0, mb

!     get interaction list
      print '(xa,$)', 'Getting interaction lists... '
      call cpu_time(t0)
      call hypoct_ilst(lvlx, nodex, chldp, nborp, nbori, ilstp, ilsti)
      call cpu_time(t)
      mb = i2mb*(size(ilstp) + size(ilsti))
      print 20, t - t0, mb

!     print output summary
      nlvl  = lvlx(2,0)
      nnode = lvlx(1,nlvl+1)
      print 30, nlvl, nnode, nborp(nnode+1), ilstp(nnode+1)

30    format(' ----------------------------------------------------', /, &
             ' Tree depth:                                 ', i8, /, &
             ' Number of nodes:                            ', i8, /, &
             ' Total number of neighbors:                  ', i8, /, &
             ' Total number of nodes in interaction lists: ', i8)

    end program