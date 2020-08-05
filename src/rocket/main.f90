program main
  !! Test the new rocket motor simulator against the legacy simulator
  implicit none
  use inflator_module, only : inflator_t
  use persistent_state_module, only : persistent_state_t
  use kind_parameters, only : rkind
  real, parameter :: tolerance=1.E-6

  interface  !interface block

    ! interface body
    function legacy_rocket() result(output)
      implicit none
      real, allocatable :: output(:,:)
    end function

  end interface

  type(inflator_t) inflator
    !! Composite abstraction encapsulating all of the relevant physics
  type(persistent_state_t) state
    !! state variables that need to be updated at each time step to allow for
    !! accumulation of values for mass, energy, time, and burn depth.
  integer file_unit

  inflator = inflator_t(input_file = "volfil.inp")

  associate( chamber_state => inflator%chamber())
     state = state_t(chamber_state%mass(), chamber_state%energy(), time = 0._rkind, burn_depth = 0._rkind)
  end associate

  open(newunit=file_unit, file="volfil.out", status="unknown")

  associate(dt => inflator%dt())
    do while(state%time() < inflator%t_max())
      associate(dState_dt => inflator%dState_dt(state))
        state = state + dt*dState_dt
        ! state_output = [output, state%output()]
      end associate
    end do
  end associate

  associate(reference_data => legacy_rocket())
    call assert( maxval(abs(state_output - reference_data)) < tolerance, &
          "main: maxval(abs(state_output - reference_data)) < tolerance")
  end associate

  print *,"Test passed."
end program
