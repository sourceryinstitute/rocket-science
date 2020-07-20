module numerics_module
  !! Encapsulate simulation numerical parameters: time step and final time.
  use kind_parameters, only : DP
  implicit none
  private

  public :: numerics_t
  public :: define
  public :: dt
  public :: t_max

  type numerics_t
    private
    real(DP) :: dt, t_max
  end type

  interface define
    module procedure define_numerics
  end interface

  interface dt
    module procedure dt_numerics
  end interface

  interface t_max
    module procedure t_max_numerics
  end interface

contains

  subroutine define_numerics(this, input_file)
     use assertions_interface, only : assert, max_errmsg_len
     type(numerics_t), intent(out) :: this
     character(len=*), intent(in) :: input_file
     character(len=max_errmsg_len) error_message
     integer io_status, file_unit
     integer, parameter :: success =0
     real(DP) dt, t_max
     namelist/numerics_list/ dt, t_max

     open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
     call assert(io_status == success, "define(numerics,...): io_status == success", error_message)
     read(file_unit, nml=numerics_list)
     close(file_unit)
     this%dt = dt
     this%t_max = t_max
  end subroutine

   function dt_numerics(this) result(this_dt)
     type(numerics_t), intent(in) :: this
     real(DP) this_dt
     this_dt = this%dt
   end function

   function t_max_numerics(this) result(this_t_max)
     type(numerics_t), intent(in) :: this
     real(DP) this_t_max
     this_t_max = this%t_max
   end function

end module numerics_module
