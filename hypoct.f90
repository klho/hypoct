!***********************************************************************
!***********************************************************************

!***********************************************************************
   module hypoct
!***********************************************************************
!***********************************************************************
    implicit none

    contains

!=======================================================================
!     User-level procedures
!=======================================================================

!***********************************************************************
     subroutine hypoct_buildx(adap, prun, term, d, n, x, s, tree)
!***********************************************************************
!***********************************************************************
      implicit none

!     ==================================================================
!     variable declarations
!     ------------------------------------------------------------------
!     arguments
      character, intent(in) :: adap, prun, term
      integer, intent(in) :: d, n, s
      real*8, intent(in) :: x(d,n)
      integer, allocatable, intent(out) :: tree(:)

!     local variables
      character, parameter :: srname = 'HYPOCT_BUILDX'
!     ==================================================================

!     handle input errors
      if ((adap /= 'a') .and. (adap /= 'u')) then
        call hypoct_errprm(srname, 'ADAP')
      endif
      if ((prun /= 'p') .and. (prun /= 'u')) then
        call hypoct_errprm(srname, 'PRUN')
      endif
      if ((term /= 'l') .and. (term /= 'o')) then
        call hypoct_errprm(srname, 'TERM')
      endif
      if (d <= 0) call hypoct_errprm(srname, 'D')
      if (n <= 0) call hypoct_errprm(srname, 'N')
      if (s <= 0) call hypoct_errprm(srname, 'S')

     end subroutine

!=======================================================================
!     Internal procedures
!=======================================================================

!***********************************************************************
     subroutine hypoct_errmsg(msg)
!***********************************************************************
!***********************************************************************
      implicit none

!     ==================================================================
!     variable declarations
!     ------------------------------------------------------------------
!     arguments
      character(len=*), intent(in) :: msg
!     ==================================================================

!     print error message
      print 10, msg
10    format(' ** ', a)

!     terminate program
      stop

     end subroutine

!***********************************************************************
     subroutine hypoct_memcpy(a, m, n)
!***********************************************************************
!***********************************************************************
      implicit none

!     ==================================================================
!     variable declarations
!     ------------------------------------------------------------------
!     arguments
      integer, intent(in) :: m
      integer, intent(out) :: n
      integer, allocatable, intent(inout) :: a(:)

!     local variables
      integer, allocatable :: b(:)
!     ==================================================================

      allocate(b(m))
      b = a(1:m)
      deallocate(a)
      allocate(a(n))
      a(1:m) = b

     end subroutine

   end module