program main
  use gas_module, only :  gas_t, define, get_c_p, get_MW, get_T, get_m
  use kind_parameters, only : DP
  implicit none
  type (gas_t) gas
  real, parameter :: expected_c_p=1000.E0, expected_MW=2.8E1, expected_T=3.E2, expected_m=0.03E0
  real, parameter :: tolerance=1.E-6

  call define(gas, "volfil.inp")

  if(abs((get_c_p(gas) - expected_c_p)/expected_c_p) > tolerance) error stop "Test Failed (cp)"
  if(abs((get_MW(gas) - expected_MW)/expected_MW) > tolerance) error stop "Test Failed (mw)"
  if(abs((get_T(gas) - expected_T)/expected_T) > tolerance) error stop "Test Failed (T)"
  if(abs((get_m(gas) - expected_m)/expected_m) > tolerance) error stop "Test Failed (m)"

  print *, "Test passed"
end program
