    module hypoct_python

     use hypoct
     implicit none
     integer, allocatable :: lvlx(:,:), nodex(:,:)
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

    end module