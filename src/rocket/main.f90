program main
  !! Test the new rocket motor simulator against the legacy simulator
  use motor_module, only : motor_t
  use state_module, only : state_t
  use kind_parameters, only : rkind
  implicit none

  real(rkind), parameter :: zero=0._rkind
  character(len=*), parameter :: input_file="rocket.inp"

  type(motor_t) motor
  type(state_t) state !! state variables updated at each time step: mass, energy, time, and burn depth
  type(state_t), allocatable :: history(:)

  call motor%define(input_file)

  associate(chamber => motor%chamber())
    call state%define(input_file, gas=chamber%gas(), volume=chamber%initial_volume(), time=zero, burn_depth=zero)
  end associate

  associate(dt => motor%dt() )
    history = [state]
    do while(state%time() < motor%t_max())
      state = state + motor%d_dt(state)*dt
      history = [history, state]
    end do
  end associate

  call write_histories
  call graph_results_if_requested

  print *,"Test passed."

contains

  subroutine graph_results_if_requested
    use command_line_interface, only : command_line_t
    type(command_line_t) command
    character(len=*), parameter :: graph(*) = [character(len=len("--graph")):: "--graph", "-g", "/graph", "/g"]

    if (command%argument_present(graph)) call execute_command_line('gnuplot gnuplot.inp')
  end subroutine

  subroutine write_histories
    use assertions_interface, only : assert, max_errmsg_len
    use results_interface, only : results_t
    character(len=*), parameter :: header(*) = [ character(len=len("burn_depth")) :: "time", "mass", "energy", "burn_depth"]
    character(len=max_errmsg_len) error_message
    integer io_status, file_unit
    integer, parameter :: success = 0

    interface
      function legacy_rocket(input_file)
        use results_interface, only : results_t
        implicit none
        character(len=*), intent(in) :: input_file
        type(results_t) legacy_rocket
      end function
    end interface

    open(newunit=file_unit, file="rocket.out", status="unknown", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "main (opening rocket.out): io_status == success", diagnostic_data=error_message)
    write(unit=file_unit, fmt=*) results_t(header, motor%derived_variables(history))
    close(file_unit)

    open(newunit=file_unit, file="legacy_rocket.out", status="unknown", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "main (opening legacy_rocket.out): io_status == success", diagnostic_data=error_message)
    write(unit=file_unit, fmt=*) legacy_rocket(input_file)
    close(file_unit)
  end subroutine

end program
