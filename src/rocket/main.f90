program main
  !! Test the new rocket motor simulator against the legacy simulator
  use motor_module, only : motor_t
  use persistent_state_module, only : persistent_state_t
  use kind_parameters, only : rkind
  implicit none

  real, parameter :: tolerance=1.E-6

  interface

    function legacy_rocket(input_file)
      use results_interface, only : results_t
      implicit none
      character(len=*), intent(in) :: input_file
      type(results_t) legacy_rocket
    end function

  end interface

  type(persistent_state_t) state
    !! state variables that need to be updated at each time step to allow for
    !! accumulation of values for mass, energy, time, and burn depth.
  type(motor_t) motor
  real(rkind), parameter :: zero=0._rkind
  character(len=*), parameter :: input_file="rocket.inp"

  call motor%define(input_file)

  associate(chamber => motor%chamber())
    call state%define(input_file, gas=chamber%gas(), volume=chamber%initial_volume(), time=zero, burn_depth=zero)
  end associate

  associate(dt => motor%dt(), t_max => motor%t_max())

    do while(state%time() < t_max)
      state = state + motor%d_dt(state)*dt
    end do

    associate(reference_results => legacy_rocket(input_file))
    end associate

  end associate

  print *,"Test passed."
end program
