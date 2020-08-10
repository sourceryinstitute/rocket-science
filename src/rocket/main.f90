program main
  !! Test the new rocket motor simulator against the legacy simulator
  use motor_module, only : motor_t
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
  type(motor_t) motor
  real(rkind), parameter :: zero=0._rkind

  call motor%define(input_file = "rocket.inp")
  associate(chamber => motor%chamber())
    call state%define(input_file="rocket.inp", gas=chamber%gas(), volume=chamber%initial_volume(), time=zero, burn_depth=zero)
  end associate

  associate(dt => motor%dt())
    do while(state%time() < motor%t_max())
      state = state + motor%d_dt(state)*dt
    end do
  end associate

  associate(reference_data => legacy_rocket())
  end associate

  print *,"Test passed."
end program
