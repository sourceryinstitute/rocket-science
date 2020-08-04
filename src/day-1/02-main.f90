program main
  !! Calculate thrust
  implicit none ! Force all entities to be declared
  real :: p=10132500., area=0.01, cf=1.0, thrust
    !! variables
  real, parameter :: thrust_expected=10132500.*0.01*1.0
    !! compile-time constant
  call calcthrust(p, area, cf, thrust)
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
