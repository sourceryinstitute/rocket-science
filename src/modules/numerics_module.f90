module numerics_module
  use kind_parameters, only : DP
  implicit none
  private

  public :: numerics_t
  public :: define
  public :: get_dt
  public :: get_tmax

  type numerics_t
  private
  real(DP) dt, tmax, time
  end type

contains

  subroutine define(this, file_name)
   use assertions_interface, only : max_errmsg_len
   type(numerics_t), intent(out) :: this
   character(len=*), intent(in) :: file_name
   character(len=max_errmsg_len) error_message
   real(DP) :: dt, tmax, time
   integer :: io_status, file_unit
   integer, parameter :: success =0
   namelist/numerics/ dt, tmax

   open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
   if (io_status /= success) error stop "chamber%define:  "
   read(file_unit, nml=numerics)
   this%dt=dt
   this%tmax=tmax
  
   close(file_unit)
  
  end subroutine

   function get_tmax(this) result(this_tmax)
     type(numerics_t), intent(in) :: this
     real(DP) :: this_tmax
     this_tmax=this%tmax
   end function

   function get_dt(this) result(this_dt)
     type(numerics_t), intent(in) :: this
     real(DP) :: this_dt, this_tmax
     this_dt=this%dt
   end function


end module numerics_module
