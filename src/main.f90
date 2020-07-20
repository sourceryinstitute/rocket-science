program volfil
  !! Test inflator burn
  use inflator_module, only : inflator_t, define, chamber, t_max, dt, dState_dt
  use persistent_state_module, only : persistent_state_t, define, time, operator(+), operator(*)
  use chamber_module, only : mass, energy
  use kind_parameters, only : DP
  implicit none
  type(inflator_t) inflator
    !! Composite abstraction encapsulating all of the relevant physics
  type(persistent_state_t) state
    !! state variables that need to be updated at each time step to allow for
    !! accumulation of values for mass, energy, time, and burn depth.

  call define(inflator, input_file = "volfil.inp")

  associate( chamber_state => chamber(inflator))
    call define(state, mass(chamber_state), energy(chamber_state), time = 0._DP, burn_depth = 0._DP)
  end associate

  associate(dt => dt(inflator))
    do while(time(state) < t_max(inflator))
      state = state + dt*dState_dt(inflator, state)
    end do
  end associate

  print *,"Test passed."
end program
