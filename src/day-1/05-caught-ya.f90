program main
  !! Calculate thrust
  use thrust_module, only : calcthrust
  implicit none ! Force all entities to be declared
  real :: p=10132500., area=0.01, cf=1.0, thrust
    !! variables
  complex :: i=(0.,1.)
  real, parameter :: thrust_expected=10132500.*0.01*1.0
    !! compile-time constant
  call calcthrust(i, area, cf, thrust)
  call assert(thrust==thrust_expected, "main: thrust==thrust_expected")

  print *,"Test passed."

  ! stop 0 ! stop code (not necessarily exit code passed to operating system)
contains
  pure subroutine assert(assertion, description)
    !! assertion utility
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (.not. assertion) error stop "Assertion ''"// trim(adjustl(description)) // "'' failed."
  end subroutine
end program
