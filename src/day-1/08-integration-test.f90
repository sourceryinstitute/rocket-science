program main
  use assertion_utility, only : assert  ! only clause documents the location of imported entities
  implicit none                         ! and prevents unnecessary name clashes and mistaken use of entitites
  real, parameter :: tolerance=1.E-6

  interface  !interface block

    ! interface body required for array function result
    function rocket() result(output)
      implicit none
      real, allocatable :: output(:,:)
    end function

    function refurbished_rocket() result(output)
      implicit none
      real, allocatable :: output(:,:)
    end function

  end interface

  associate( &                                 ! Fortran 2003 feature
    reference_data => rocket(), &              ! an associate name that is associated with a function result or expression
    refurbished_data => refurbished_rocket() & ! is immutable
  )
    call assert(maxval(abs(refurbished_data - reference_data))<= tolerance, &
         "main: maxval(abs(refurbished_data - reference_data))<= tolerance")
  end associate
  print *,"Test passed."
end program
