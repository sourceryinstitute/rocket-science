   program main
    use pyro_module, only : pyro_t, define, get_burn_rate_exp, get_gas_yield
    use kind_parameters, only : DP
    implicit none
    type(pyro_t) pyro
    real(DP), parameter :: expected_n=0.4e0, expected_y=0.7e0, tolerance=1.E-06
   
    call define(pyro, "volfil.inp")
   
    if ( abs((get_burn_rate_exp(pyro) - expected_n)/expected_n) > tolerance) error stop "Test failed."
  
    if ( abs((get_gas_yield(pyro) - expected_y)/expected_y) > tolerance) error stop "Test failed."

    print *,"Test passed."
   end program
