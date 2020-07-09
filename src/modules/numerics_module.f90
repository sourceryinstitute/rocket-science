module numerics_module
  use kind_parameters, only : DP
  implicit none
  private

  public :: numerics_t
  public :: define
  public :: get_dt
  public :: get_tmax
  public :: get_time
  public :: set_time
  public :: operator(+)
  public :: operator(*)
  public :: d_dt

  type numerics_t
    private
    real(DP) dt, tmax, time
  end type

  interface operator(+)
    module procedure add
  end interface

  interface operator(*)
    module procedure multiply
  end interface

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

   function get_dt(this) result(this_dt)
     type(numerics_t), intent(in) :: this
     real(DP) :: this_dt, this_tmax
     this_dt=this%dt
   end function

   function get_tmax(this) result(this_tmax)
     type(numerics_t), intent(in) :: this
     real(DP) :: this_tmax
     this_tmax=this%tmax
   end function

   function get_time(this) result(this_time)
     type(numerics_t), intent(in) :: this
     real(DP) this_time
     this_time = this%time
   end function

   subroutine set_time(this, time)
     type(numerics_t), intent(inout) :: this
     real(DP), intent(in) :: time
     this%time = time
   end subroutine

   function d_dt(this) result(dthis_dt)
     type(numerics_t), intent(in) :: this
     type(numerics_t) dthis_dt
     dthis_dt%dt = 0._DP
     dthis_dt%tmax = 0._DP
     dthis_dt%time = 1._DP
   end function

   function add(lhs, rhs) result(total)
     type(numerics_t), intent(in) :: lhs, rhs
     type(numerics_t) total
     total%dt = lhs%dt + rhs%dt
     total%tmax = lhs%tmax + rhs%tmax
     total%time = lhs%time + rhs%time
   end function

   function multiply(lhs, rhs) result(product)
     type(numerics_t), intent(in) :: lhs
     real(DP), intent(in) :: rhs
     type(numerics_t) product
     product%dt = lhs%dt * rhs
     product%tmax = lhs%tmax * rhs
     product%time = lhs%time * rhs
   end function

end module numerics_module
