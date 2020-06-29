program main
  use assertions_interface, only : assert
  use gas_module, only :  gas_t, define, get_c_p, get_MW, get_T, get_m
  use kind_parameters, only : DP
  implicit none
  type (gas_t) gas
  real(DP), parameter :: expected_c_p=1000.E0_DP, expected_MW=2.8E1_DP, expected_T=3.E2_DP, expected_m=0.03E0_DP
  real(DP), parameter :: tolerance=1.E-6_DP

  call define(gas, "volfil.inp")

  call assert(abs((get_c_p(gas) - expected_c_p)/expected_c_p) <= tolerance, &
             "abs((get_c_p(gas) - expected_c_p)/expected_c_p) <= tolerance)")
  call assert(abs((get_MW(gas)  - expected_MW )/expected_MW ) <= tolerance, &
             "abs((get_MW(gas)  - expected_MW )/expected_MW ) <= tolerance)")
  call assert(abs((get_T(gas)   - expected_T  )/expected_T  ) <= tolerance, &
             "abs((get_T(gas)   - expected_T  )/expected_T  ) <= tolerance)")
  call assert(abs((get_m(gas)   - expected_m  )/expected_m  ) <= tolerance, &
             "abs((get_m(gas)   - expected_m  )/expected_m  ) <= tolerance)")

  print *, "Test passed"
end program
