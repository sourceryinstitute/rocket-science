program main
  use assertions_interface, only : assert
  use chamber_module, only : chamber_t, define, get_volume
  use kind_parameters, only : DP
  implicit none
  type(chamber_t) chamber
  real(DP), parameter :: expected_volume=10.E-04_DP, tolerance=1.E-06_DP

  call define(chamber, "volfil.inp")

  call assert(abs((get_volume(chamber) - expected_volume)/expected_volume) <= tolerance, &
             "abs((get_volume(chamber) - expected_volume)/expected_volume) <= tolerance")

  print *,"Test passed."
end program
