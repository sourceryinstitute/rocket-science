program volfil
  !! Test composite inflator abstraction
  use inflator_module, only : inflator_t, define, chamber, t_max, dt, dState_dt
  use persistent_state_module, only : persistent_state_t, define, time, operator(+), operator(*)
  use chamber_module, only : mass, energy
  use kind_parameters, only : DP
  implicit none
  type(inflator_t) inflator
  type(persistent_state_t) state

  call define(inflator, input_file = "volfil.inp")

  associate( chamber_state => chamber(inflator))
    call define(state, mass(chamber_state), energy(chamber_state), time = 0._DP, burn_depth = 0._DP)
  end associate

  associate(final_time => t_max(inflator), dt => dt(inflator))
    do while(time(state) < final_time)
      state = state + dt*dState_dt(inflator, state)
    end do
  end associate

  print *,"Test passed."
end program
