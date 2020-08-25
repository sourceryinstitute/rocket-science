program main
  !! Test the new rocket motor simulator against the legacy simulator
  use assertions_interface, only : assert, max_errmsg_len
  use motor_module, only : motor_t
  use state_module, only : state_t
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

  type(motor_t) motor
  type(state_t) state !! state variables updated at each time step: mass, energy, time, and burn depth

  real(rkind), parameter :: zero=0._rkind
  character(len=*), parameter :: input_file="rocket.inp"

  call motor%define(input_file)

  associate(chamber => motor%chamber())
    call state%define(input_file, gas=chamber%gas(), volume=chamber%initial_volume(), time=zero, burn_depth=zero)
  end associate

  associate(dt => motor%dt() )

    do while(state%time() < motor%t_max())
      state = state + motor%d_dt(state)*dt
    end do

    associate(reference_results => legacy_rocket(input_file))
      block
        use command_line_interface, only : command_line_t
        type(command_line_t) command
        character(len=max_errmsg_len) error_message
        integer io_status, file_unit
        integer, parameter :: success = 0

        if (command%argument_present([character(len=len("--graph")):: "--graph", "-g", "/graph", "/g"])) then
          open(newunit=file_unit, file="rocket.out", status="unknown", iostat=io_status, iomsg=error_message)
          call assert(io_status == success, "main: io_status == success", diagnostic_data=error_message)
          write(unit=file_unit, fmt=*) reference_results
          close(file_unit)
          call execute_command_line('gnuplot gnuplot.inp')
        end if
      end block
    end associate
  end associate

  print *,"Test passed."
end program
