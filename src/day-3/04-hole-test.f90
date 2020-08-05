! Compile and run:
!
! cp 02-object_interface.f90 05-hole_interface.f90
! cp 03-object_implementation.f90 06-hole_implementation.f90
!
! Edit 05-hole_interface.f90
! Edit 06-hole_implementation.f90
!
! gfortran -c 05-hole_interface.f90
! gfortran -c 06-hole_implementation.f90
! gfortran -c 04-hole-test.f90
! gfortran *.o -o hole-test
! ./hole-test
!
program main
  use hole_interface, only : hole_t
  implicit none

  real, parameter :: pi=3.141592654, dia=2.

  type(hole_t) hole

  call hole%set_diameter( diameter=dia )

  call assert(hole%area() == pi*(dia**2)/4.)

  print *,"Test passed."
contains

  subroutine assert(assertion)
    logical, intent(in) :: assertion
    if (.not. assertion) error stop "Test failed."
  end subroutine

end program

