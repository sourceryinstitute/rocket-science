program main
  use assertions_interface, only : assert
  use numerics_module, only : numerics_t, define, get_dt, get_tmax
  use kind_parameters, only : DP
  implicit none
  type(numerics_t) numerics
  real(DP), parameter :: expected_dt=1e-6_DP, expected_tmax=0.1e0_DP, tol=1.e-6_DP

  call define(numerics, "volfil.inp")

  call assert(abs((get_dt(numerics)-expected_dt)/expected_dt) <= tol, &
             "abs((get_dt(numerics)-expected_dt)/expected_dt) <= tol,")
  call assert(abs((get_tmax(numerics)-expected_tmax)/expected_tmax) <= tol, &
             "abs((get_tmax(numerics)-expected_tmax)/expected_tmax) <= tol")

  print *, "Test passed."
end program main
