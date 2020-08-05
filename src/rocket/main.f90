program main
  !! Test the new rocket motor simulator against the legacy simulator
  use inflator_module, only : inflator_t
  use persistent_state_module, only : persistent_state_t
  use kind_parameters, only : rkind
  implicit none

  real, parameter :: tolerance=1.E-6

  interface  !interface block

    function legacy_rocket() result(output)
      !! interface body for reference implementation
      implicit none
      real, allocatable :: output(:,:)
    end function

  end interface

  type(persistent_state_t) state
    !! state variables that need to be updated at each time step to allow for
    !! accumulation of values for mass, energy, time, and burn depth.

  associate(inflator => inflator_t(input_file = "rocket.inp"))

    associate( chamber_state => inflator%chamber())
      state = persistent_state_t(chamber_state%mass(), chamber_state%energy(), time = 0._rkind, burn_depth = 0._rkind)
    end associate

    associate(dt => inflator%dt())
      do while(state%time() < inflator%t_max())
        associate(dState_dt => inflator%d_dt(state))
          state = state + dt*dState_dt
        end associate
      end do
    end associate

    associate(reference_data => legacy_rocket())
    end associate

  end associate

  print *,"Test passed."
end program
