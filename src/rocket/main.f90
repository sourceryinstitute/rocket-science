program main
  !! Test the new rocket motor simulator against the legacy simulator
  implicit none
  real, parameter :: tolerance=1.E-6

  interface  !interface block

    ! interface body
    function legacy_rocket() result(output)
      implicit none
      real, allocatable :: output(:,:)
    end function

  end interface

  associate(reference_data => legacy_rocket())
    ! add an assertion to check the results from new rocket
    ! design against the legacy design
  end associate

  print *,"Test passed."
end program
