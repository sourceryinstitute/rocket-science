program main
  use assertion_utility, only : assert
  implicit none
  real, parameter :: tolerance=1.E-6

  interface  !interface block

    ! interface body
    function rocket() result(output)
      implicit none
      real, allocatable :: output(:,:)
    end function

    function refurbished_rocket() result(output)
      implicit none
      real, allocatable :: output(:,:)
    end function

  end interface

  associate( &
    reference_data => rocket(), &
    refurbished_data => refurbished_rocket() &
  )
    call assert(maxval(abs(refurbished_data - reference_data))<= tolerance, &
         "main: maxval(abs(refurbished_data - reference_data))<= tolerance")
  end associate
  print *,"Test passed."
end program
