program volfil
  !! Test composite inflator abstraction
  use inflator_module, only : inflator_t, define, t_max, step
  use persistent_state_module, only : persistent_state_t, define, time
  use kind_parameters, only : DP
  implicit none
  type(inflator_t) inflator
  type(persistent_state_t) state

  call define(inflator, input_file = "volfil.inp")
  call define(state, time = 0._DP, burn_depth = 0._DP)

  associate(final_time => t_max(inflator))
    do while(time(state) < final_time)
      state = step(inflator, state)
    end do
  end associate

  print *,"Test passed."
end program
