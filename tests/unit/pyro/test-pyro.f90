program main
  use assertions_interface, only : assert
  use pyro_module, only : pyro_t, define, get_burn_rate_exp, get_gas_yield
  use kind_parameters, only : DP
  implicit none
  type(pyro_t) pyro
  real(DP), parameter :: expected_n=0.4e0_DP, expected_y=0.7e0_DP, tolerance=1.E-06_DP

  call define(pyro, "volfil.inp")

  call assert(abs((get_burn_rate_exp(pyro) - expected_n)/expected_n) <= tolerance, &
             "abs((get_burn_rate_exp(pyro) - expected_n)/expected_n) <= tolerance")
  call assert(abs((get_gas_yield(pyro) - expected_y)/expected_y) <= tolerance, &
             "abs((get_gas_yield(pyro) - expected_y)/expected_y) <= tolerance")

  print *,"Test passed."
end program
