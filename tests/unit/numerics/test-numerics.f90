program main
  use assertions_interface, only : assert
  use numerics_module, only : numerics_t, define, get_dt, get_tmax, get_time, set_time, operator(+), operator(*), d_dt
  use kind_parameters, only : DP
  implicit none
  type(numerics_t) numerics
  real(DP), parameter :: expected_dt=1e-6_DP, expected_tmax=0.1e0_DP, tol=1.e-6_DP

  call define(numerics, "volfil.inp")

  call assert(abs((get_dt(numerics)-expected_dt)/expected_dt) <= tol, &
             "abs((get_dt(numerics)-expected_dt)/expected_dt) <= tol,")
  call assert(abs((get_tmax(numerics)-expected_tmax)/expected_tmax) <= tol, &
             "abs((get_tmax(numerics)-expected_tmax)/expected_tmax) <= tol")

  associate(t => get_time(numerics), dt => get_dt(numerics))

    call set_time(numerics, t + dt)
    call assert(abs((get_time(numerics)-(t+dt)))/(t+dt) <= tol, &
               "abs((get_time(numerics)-(t+dt)))/(t+dt) <= tol")

    numerics = numerics + d_dt(numerics)*dt
    call assert(abs((get_time(numerics)-(t+2*dt)))/(t+2*dt) <= tol, &
               "abs((get_time(numerics)-(t+2*dt)))/(t+2*dt) <= tol")

  end associate

  print *, "Test passed."
end program main
