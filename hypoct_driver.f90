    program hypoct_driver

      use hypoct
      implicit none

      character :: adap, intr
      integer :: d, n, occ, lvlmax
      integer, allocatable :: lvlx(:,:), xi(:), nodex(:,:)
      real*8, allocatable :: x(:,:), siz(:), ext(:), rootx(:,:), l(:,:), ctr(:,:)

      adap = 'a'
      intr = 'p'
      d = 1
      n = 9
      allocate(x(d,n), siz(n), ext(d), rootx(2,d), xi(n))
      call random_number(x)
      siz = 0
      occ = 1
      lvlmax = -1
      ext = 0

      call hypoct_buildx(adap, intr, d, n, x, siz, occ, lvlmax, ext, lvlx, &
                         rootx, xi, nodex)

      print '(2(i8))', lvlx
      print *
      print '(3(i8))', nodex
      print *

      call hypoct_geom(d, lvlx, rootx, nodex, l, ctr)

      print '(1(e12.4))', l
      print *
      print '(1(e12.4))', ctr

    end program