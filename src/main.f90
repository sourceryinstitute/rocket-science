program volfil
  !! Test inflator burn
  use inflator_module, only : inflator_t, define, output, chamber, t_max, dt, dState_dt
  use persistent_state_module, only : persistent_state_t, define, time, operator(+), operator(*), output
  use chamber_module, only : mass, energy
  use kind_parameters, only : DP
  implicit none
  type(inflator_t) inflator
    !! Composite abstraction encapsulating all of the relevant physics
  type(persistent_state_t) state
    !! state variables that need to be updated at each time step to allow for
    !! accumulation of values for mass, energy, time, and burn depth.
   integer file_unit

  call define(inflator, input_file = "volfil.inp")

  associate( chamber_state => chamber(inflator))
    call define(state, mass(chamber_state), energy(chamber_state), time = 0._DP, burn_depth = 0._DP)
  end associate

  open(newunit=file_unit, file="volfil.out", status="unknown")

  associate(dt => dt(inflator))
    do while(time(state) < t_max(inflator))
      associate(dState_dt => dState_dt(inflator, state))
        call output(dState_dt, file_unit)
        !call output(inflator, time(state), file_unit)
        state = state + dt*dState_dt
      end associate
    end do
  end associate

  print *,"Test passed."
end program
