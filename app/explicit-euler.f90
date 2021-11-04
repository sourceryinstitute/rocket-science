program explicit_euler
  !! A mini-application for rocket motor simulation:
  !! demonstrating an object-oriented, functional programming style in Fortran 2018
  !! with automatic graphing of results in gnuplot for comparison against a legacy,
  !! procedural simulator written in Fortran 90.
  use motor_interface, only : motor_t
  use state_interface, only : state_t
  use kind_parameters, only : rkind
  implicit none

  real(rkind), parameter :: zero=0._rkind
  character(len=*), parameter :: input_file="app/rocket.inp"

  type(motor_t) motor
  type(state_t) state !! state variables updated at each time step: mass, energy, time, and burn depth
  type(state_t), allocatable :: history(:)

  call motor%define(input_file)

  associate(chamber => motor%chamber())
    associate(gas => chamber%gas())
      call state%define(input_file, R_gas=gas%R_gas(), c_v=gas%c_v(), volume=chamber%initial_volume(), time=zero, burn_depth=zero)
    end associate
  end associate

  associate(dt => motor%dt() )
    history = [state]
    do while(state%time() < motor%t_max())
      state = state + motor%d_dt(state)*dt
      history = [history, state]
    end do
  end associate

  call write_results
  call graph_if_requested

contains

  subroutine write_history(results, file_name)
    use assertions_interface, only : assert, max_errmsg_len
    use results_interface, only : results_t
    type(results_t), intent(in) :: results
    character(len=*), intent(in) :: file_name
    character(len=max_errmsg_len) error_message
    integer io_status, file_unit
    integer, parameter :: success = 0

    open(newunit=file_unit, file=file_name, status="unknown", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "main (opening "//file_name//"): io_status == success", diagnostic_data=error_message)
    write(unit=file_unit, fmt=*) results
    close(file_unit)
  end subroutine

  subroutine write_results
    use results_interface, only : results_t
    use refurbished_rocket_module, only : refurbished_rocket
    character(len=*), parameter :: header(*) = &
        [character(len=len("temperature")) :: "time", "pressure", "temperature", "mdotos", "thrust", "volume"]

    interface
     function legacy_rocket(input_file)
       import results_t
       implicit none
       character(len=*), intent(in) :: input_file
       type(results_t) legacy_rocket
     end function
   end interface

    associate( &
      modern_results => results_t(header, motor%derived_variables(history)), &
      legacy_results => legacy_rocket(input_file), &
      refurbished_results => refurbished_rocket(input_file) &
    )
      call write_history(modern_results, "rocket.out")
      call write_history(legacy_results, "legacy_rocket.out")
      call write_history(refurbished_results, "refurbished_rocket.out")
    end associate
  end subroutine

  subroutine graph_if_requested()
    use command_line_interface, only : command_line_t
    type(command_line_t) command
    character(len=*), parameter :: graph(*) = [character(len=len("--graph")):: "--graph", "-g", "/graph", "/g"]

    if (command%argument_present(graph)) call execute_command_line('gnuplot app/gnuplot.inp')
  end subroutine
end program
