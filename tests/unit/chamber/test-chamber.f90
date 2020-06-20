program main
  use chamber_module, only : chamber_t, define, get_volume
  use kind_parameters, only : DP
  implicit none
  type(chamber_t) chamber
  real, parameter :: expected_volume=10.E-04_DP, tolerance=1.E-06

  call define(chamber, "volfil.inp")

  if ( abs((get_volume(chamber) - expected_volume)/expected_volume) > tolerance)  error stop "Test failed."

  print *,"Test passed."
end program
