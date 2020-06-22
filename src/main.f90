program main
  implicit none

  type numerics
    real :: dt
  end type

  type gas_t
    real :: c_p, MW, T, m
    real :: c_v, rgas, g  ! cv, gas constant, gamma
  end type

  type pyro_t
    real :: m, height, diameter, gas_yield, density, flame_temp, burn_rate_ref, burn_rate_exp
    real :: num_tablets
  end type

  type hole_t
   ! real, parameter :: (cd=0.75)
    real :: diameter
  end type

  type chamber_t
    real volume
    type(hole_t), allocatable :: hole(:)
  end type

  type inflator_t
    real :: time
    type(gas_t) :: gas
    type(pyro_t) :: pyro
    type(chamber_t) :: chamber
  end type

end program main
