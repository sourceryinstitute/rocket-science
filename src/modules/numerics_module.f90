module numerics_module
  use assertions_interface, only : assert
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

  type numerics_t
    private
    real(DP) time
  end type

  real(DP) dt, tmax

  interface set_time
    module procedure set_time_numerics
  end interface

  interface get_time
    module procedure get_time_numerics
  end interface

  interface get_tmax
    module procedure get_tmax_numerics
  end interface

  interface define
    module procedure define_numerics
  end interface

  interface operator(+)
    module procedure add_real_scalar
  end interface

  interface operator(*)
    module procedure multiply
  end interface

contains

  subroutine define_numerics(this, file_name)
     use assertions_interface, only : max_errmsg_len
     type(numerics_t), intent(out) :: this
     character(len=*), intent(in) :: file_name
     character(len=max_errmsg_len) error_message
     integer io_status, file_unit
     integer, parameter :: success =0
     namelist/numerics_list/ dt, tmax

     open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
     call assert(io_status == success, "define(numerics): io_status == success", error_message)
     read(file_unit, nml=numerics_list) ! set dt, tmax
     close(file_unit)
  end subroutine

   function get_dt(this) result(module_dt)
     type(numerics_t), intent(in) :: this
     real(DP) module_dt
     module_dt = dt
   end function

   function get_tmax_numerics(this) result(module_tmax)
     type(numerics_t), intent(in) :: this
     real(DP) module_tmax
     module_tmax = tmax
   end function

   function get_time_numerics(this) result(this_time)
     type(numerics_t), intent(in) :: this
     real(DP) this_time
     this_time = this%time
   end function

   subroutine set_time_numerics(this, time)
     type(numerics_t), intent(inout) :: this
     real(DP), intent(in) :: time
     this%time = time
   end subroutine

   function add_real_scalar(lhs, rhs) result(total)
     type(numerics_t), intent(in) :: lhs
     real(DP), intent(in) :: rhs
     type(numerics_t) total
     total%time = lhs%time + rhs
   end function

   function multiply(lhs, rhs) result(product)
     type(numerics_t), intent(in) :: lhs
     real(DP), intent(in) :: rhs
     type(numerics_t) product
     product%time = lhs%time * rhs
   end function

end module numerics_module
