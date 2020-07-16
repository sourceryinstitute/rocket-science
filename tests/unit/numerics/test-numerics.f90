program main
  !! Test the numerics module's type, procedures, and operators
  use assertions_interface, only : assert, max_errmsg_len
  use numerics_module, only : numerics_t, define, dt, t_max
  use kind_parameters, only : DP
  implicit none
  type(numerics_t) numerics
  real(DP), parameter :: tol=1.e-6_DP
  character(len=*), parameter :: file_name="volfil.inp"
  real(DP) :: dt_main, t_max_main

  call read_numerics(file_name, dt_main, t_max_main)

  call define(numerics, file_name)
  call assert(abs((dt(numerics)-dt_main)/dt_main) <= tol, &
             "abs((dt(numerics)-dt_main)/dt_main) <= tol")
  call assert(abs((t_max(numerics)-t_max_main)/t_max_main) <= tol, &
             "abs((t_max(numerics)-t_max_main)/t_max_main) <= tol")

  print *, "Test passed."

contains

  subroutine read_numerics(input_file, dt, t_max)
    character(len=*), intent(in) :: input_file
    real(DP), intent(out) ::  dt, t_max
    integer io_status, file_unit
    integer, parameter :: success =0
    character(len=max_errmsg_len) error_message
    namelist/numerics_list/ dt, t_max

    open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "io_status == success", error_message)
    read(file_unit, nml=numerics_list)
    close(file_unit)
  end subroutine

end program main
