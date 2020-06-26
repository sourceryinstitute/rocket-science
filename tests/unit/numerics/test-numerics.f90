program main
 use numerics_module, only : numerics_t, define, get_dt, get_tmax
 use kind_parameters, only : DP

     implicit none
     type(numerics_t) numerics
     real(DP), parameter :: expected_dt=1e-6, expected_tmax=0.1e0, tol=1.e-6

     call define(numerics, "volfil.inp")

     if(abs((get_dt(numerics)-expected_dt)/expected_dt) > tol) error stop "test failed"
 
     if(abs((get_tmax(numerics)-expected_tmax)/expected_tmax) > tol) error stop "test failed"
    print *, "Test passed."
end program main
