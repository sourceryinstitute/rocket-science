program main
  !! Reference, procedural rocket motor simulation
  implicit none

  interface

    function legacy_rocket(input_file)
      use results_interface, only : results_t
      use mod1, only : dp
      implicit none
      character(len=*), intent(in) :: input_file
      type(results_t) legacy_rocket
    end function

  end interface

  character(len=*), parameter :: input_file="rocket.inp"

  block
    use results_interface, only : results_t
    type(results_t) reference_results

    reference_results = legacy_rocket(input_file)
    print *, reference_results
  end block

  print *," Test passed."

end program main
