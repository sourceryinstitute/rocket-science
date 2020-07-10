program main
  !! Test the numerics module's type, procedures, and operators
  use assertions_interface, only : assert, max_errmsg_len
  use numerics_module, only : numerics_t, define, get_dt, get_tmax, get_time, set_time, operator(+), operator(*), d_dt
  use kind_parameters, only : DP
  implicit none
  type(numerics_t) numerics
  real(DP), parameter :: tol=1.e-6_DP
  character(len=max_errmsg_len) error_message
  character(len=*), parameter :: file_name="volfil.inp"
  real(DP) :: dt, tmax
  integer :: io_status, file_unit
  integer, parameter :: success =0
  namelist/numerics_list/ dt, tmax

  ! Read dt and tmax from file
  open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
  call assert(io_status == success, "io_status == success", error_message)
  read(file_unit, nml=numerics_list)
  close(file_unit)

  ! Test define subroutine: check that it sets and stores the dt & tmax values defined in the input file
  call define(numerics, file_name) !! initialize numerics object with data read from file
  call assert(abs((get_dt(numerics)-dt)/dt) <= tol, &
             "abs((get_dt(numerics)-dt)/dt) <= tol")
  call assert(abs((get_tmax(numerics)-tmax)/tmax) <= tol, &
             "abs((get_tmax(numerics)-tmax)/tmax) <= tol")

  associate(t => get_time(numerics), dt => get_dt(numerics)) ! Fortran 2003
    !! associate names with numerics function results to
    !! 1. Eliminate the need for a declaration (making code more compact).
    !! 2. Eliminate one class of mistakes (using the variable in a way that is problematic given its declared type).
    !! 3. Enable compiler optimizations (because t & dt are immutable, expressions like t+dt need be evaluated only once).

    block
      integer i
      integer, parameter :: loop_iterations=3

      do i = 1, loop_iterations
        call set_time(numerics, t + i*dt) !! increment time by dt using setter
        call assert(abs((get_time(numerics)-(t+i*dt)))/(t+i*dt) <= tol, &
                   "abs((get_time(numerics)-(t+i*dt)))/(t+i*dt) <= tol")

        numerics = numerics + d_dt(numerics)*dt !! increment time by dt using user-defined operators
        call assert(abs((get_time(numerics)-(t+(i+1)*dt)))/(t+(i+1)*dt) <= tol, &
                   "abs((get_time(numerics)-(t+(i+1)*dt)))/(t+(i+1)*dt) <= tol")
      end do
    end block

  end associate

  print *, "Test passed."
end program main
