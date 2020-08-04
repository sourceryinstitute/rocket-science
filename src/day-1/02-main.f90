program main
  !! Unit test for calcthrust
  implicit none ! Force all entities to be declared

  real :: p=10132500., area=0.01, cf=1.0, thrust
    !! variables declared locally provides information about the variables
    !! without needing to search the module
  real, parameter :: thrust_expected=10132500.*0.01*1.0
    !! compile-time constant -- avoid "magic numbers"

  call calcthrust(p, area, cf, thrust)

  call assert(thrust==thrust_expected, "main: thrust==thrust_expected")

  ! Alternative:
  ! if (thrust/=thrust_expected) error stop "unexpected thrust"

  print *,"Test passed." ! cmake searches for this string to determine whether the test passed

  ! stop 0 ! stop code (not necessarily exit code passed to operating system)

contains

  ! Fortran features:
  ! 1. Pure procedure (Fortran 95)
  ! 2. Error stop in pure procedure (Fortran 2018)
  ! 3. Variable stop code (Fortran 2018)

  ! For a description of pure Fortran 2018 standard, Note 4 on p. 325

  pure subroutine assert(assertion, description)
    !! assertion "utility" written as an internal subroutine
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description

    ! real :: y=1. ! not allowed in a pure subprogram because of the implicit save attribute

    ! real, parameter :: x=1.
    ! p = x ! not allowed in a pure subprogram because it modifies a host-associated variable

    ! print *,p ! not allowed in a pure subprogram

    if (.not. assertion) error stop "Assertion '"// trim(adjustl(description)) // "' failed."

  end subroutine

end program
