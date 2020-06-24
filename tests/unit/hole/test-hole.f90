program main
  use hole_module, only :  hole_t, define, get_diameter
  use kind_parameters, only : DP
  implicit none
  type (hole_t) hole
  real, parameter :: expected_diameter=9.E-3
  real, parameter :: tolerance=1.E-6

  call define(hole, "volfil.inp")

  if(abs((get_diameter(hole) - expected_diameter)/expected_diameter) > tolerance) error stop "Test Failed (hole)"


  print *, "Test passed"
end program




