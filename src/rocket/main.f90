program main
  use iso_fortran_env, only : real64
  implicit none

  interface

    function rocket() result(output)
      real(8), allocatable :: output(:,:)
    end function

  end interface

  associate(reference_data => rocket())
  end associate

end program
