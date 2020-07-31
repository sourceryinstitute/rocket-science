program main
  use iso_fortran_env, only : real64
  implicit none

  integer i

  interface

    function reference_rocket() result(output)
      real(8), allocatable :: output(:,:)
    end function

  end interface

  associate(reference_data => reference_rocket())
    do i=1,size(reference_data,1)
      print *,reference_data(i,1), reference_data(i,2)
    end do
  end associate

end program
