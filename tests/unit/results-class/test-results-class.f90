program main
  !! Test results abstraction
  use results_interface, only : results_t
  implicit none

  interface

    function legacy_rocket(input_file)
      use results_interface, only : results_t
      implicit none
      character(len=*), intent(in) :: input_file
      type(results_t) legacy_rocket
    end function

  end interface

  type(results_t) reference_results

  reference_results = legacy_rocket(input_file="rocket.inp")
  print *, reference_results

  print *," Test passed."

end program main
