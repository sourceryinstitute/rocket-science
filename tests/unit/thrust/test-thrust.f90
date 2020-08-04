program main
  !! Legacy thrust calculation unit test
  use mod1
  implicit none ! Force all entities to be declared

  p=10132500.
  area=0.01
  cf=1.0

  call calcthrust
  call assert(thrust==p*area*cf, "main: thrust==thrust_expected")

  print *,"Test passed."

contains

  ! Fortran features employed:
  ! 1. Pure attrivute (Fortran 95)
  ! 2. Variable stop code (Fortran 2018)
  ! 3. Error stop inside a pure procedure (Fortran 2018)

  pure subroutine assert(assertion, description)
    !! assertion utility
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (.not. assertion) error stop "Assertion ''"// trim(adjustl(description)) // "'' failed."
  end subroutine

end program
