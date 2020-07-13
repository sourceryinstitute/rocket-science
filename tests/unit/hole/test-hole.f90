program main
  use assertions_interface, only : assert
  use hole_module, only :  hole_t, define, get_diameter, area
  use kind_parameters, only : DP
  implicit none
  type(hole_t) hole
  real(DP), parameter :: expected_diameter=0.018_DP, tolerance=1.E-6_DP, pi=3.141592654_DP

  call define(hole, "volfil.inp")

  call assert(abs((get_diameter(hole) - expected_diameter)/expected_diameter) <= tolerance, &
             "abs((get_diameter(hole) - expected_diameter)/expected_diameter) <= tolerance")

  associate(expected_area => pi*expected_diameter**2/4._DP)
    call assert(abs(area(hole) - expected_area)/expected_area <= tolerance, &
               "abs(area(hole) - expected_area)/expected_area <= tolerance")
  end associate

  print *, "Test passed"
end program
