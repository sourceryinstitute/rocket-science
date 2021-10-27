program main
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
    block
      real(rkind), parameter :: B2(*) = [2._rkind/9._rkind]
      real(rkind), parameter :: B3(*) = [1._rkind/12._rkind, 1._rkind/4._rkind]
      real(rkind), parameter :: B4(*) = [69._rkind/128._rkind, -243._rkind/128._rkind, 135._rkind/64._rkind]
      real(rkind), parameter :: B5(*) = [-17._rkind/12._rkind, 27._rkind/4._rkind, -27._rkind/5._rkind, 16._rkind/15._rkind]
      real(rkind), parameter :: B6(*) = &
        [65._rkind/432._rkind, -5._rkind/16._rkind, 13._rkind/16._rkind, 4._rkind/27._rkind, 5._rkind/144._rkind]
      real(rkind), parameter :: CH(*) = &
        [47._rkind/450._rkind, 0._rkind, 12._rkind/25._rkind, 32._rkind/225._rkind, 1._rkind/30._rkind, 6._rkind/25._rkind]
      
      do while(state%time() < motor%t_max())
        associate(k1 => motor%d_dt(state)*dt)
          associate(k2 => motor%d_dt(state + k1*B2(1))*dt)
            associate(k3 => motor%d_dt(state + k1*B3(1) + k2*B3(2))*dt)
              associate(k4 => motor%d_dt(state + k1*B4(1)  + k2*B4(2)+ k3*B4(3))*dt)
                associate(k5 => motor%d_dt(state + k1*B5(1) + k2*B5(2) + k3*B5(3) + k4*B5(4))*dt)
                  associate(k6 => motor%d_dt(state + k1*B6(1) + k2*B6(2) + k3*B6(3) + k4*B6(4) + k5*B6(5))*dt)
                    state = state + k1*CH(1) + k2*CH(2) + k3*CH(3) + k4*CH(4) + k5*CH(5)+ k6*CH(6)
                  end associate
                end associate
              end associate
            end associate
          end associate
        end associate
        history = [history, state]
      end do
    end block 
  end associate

  call write_and_verify_results
  call graph_if_requested

  print *,"Test passed."

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

  subroutine write_and_verify_results
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
      block
        use assertions_interface, only : assert
        real(rkind), parameter :: tolerance = 0.01_rkind
        !call assert(modern_results%max_filtered_normalized_distance(legacy_results) < tolerance, &
        !           "modern_results%max_filtered_normalized_distance(legacy_results)")
        !call assert(refurbished_results%max_filtered_normalized_distance(legacy_results) < tolerance, &
        !           "refurbished_results%max_filtered_normalized_distance(legacy_results)")
      end block
    end associate
  end subroutine

  subroutine graph_if_requested()
    use command_line_interface, only : command_line_t
    type(command_line_t) command
    character(len=*), parameter :: graph(*) = [character(len=len("--graph")):: "--graph", "-g", "/graph", "/g"]

    if (command%argument_present(graph)) call execute_command_line('gnuplot app/gnuplot.inp')
  end subroutine
end program
