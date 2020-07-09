program main
  use assertions_interface, only : assert
  use hole_module, only :  hole_t, define, get_diameter
  use kind_parameters, only : DP
  implicit none
  type (hole_t) hole
  real(DP), parameter :: expected_diameter=9.E-3_DP
  real(DP), parameter :: tolerance=1.E-6_DP

  call define(hole, "volfil.inp")

  call assert(abs((get_diameter(hole) - expected_diameter)/expected_diameter) <= tolerance, &
             "abs((get_diameter(hole) - expected_diameter)/expected_diameter) <= tolerance")

  print *, "Test passed"
end program




