program main
  implicit none

  type inflator_t
  end type



  type numerics_t
    real :: dt, time, tmax
  end type

  type gas_t
    real :: c_p, MW, c_v, rgas, g
  end type

  type pyro_t
    real :: mgen, height, diameter, gas_yield, density, flame_temp, burn_rate_ref, burn_rate_exp
    real :: num_tablets, burn_dist, mdotgen, edotgen
  end type

  type chamber_t
    real volume, mgas, E, M, edot, mdot, T, P, diam
  end type


end program main
