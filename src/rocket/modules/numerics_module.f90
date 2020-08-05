module numerics_module
  !! Encapsulate simulation numerical parameters: time step and final time.
  use kind_parameters, only : rkind
  implicit none

  private
  public :: numerics_t

  type numerics_t
    private
    real(rkind) :: dt_, t_max_
  contains
    procedure :: dt
    procedure :: t_max
  end type

  interface numerics_t
    module procedure construct_numerics_t
  end interface

contains

  function construct_numerics_t(input_file) result(new_numerics_t)
     use assertions_interface, only : assert, max_errmsg_len
     type(numerics_t) new_numerics_t
     character(len=*), intent(in) :: input_file
     character(len=max_errmsg_len) error_message
     integer io_status, file_unit
     integer, parameter :: success =0
     real(rkind) dt, t_max
     namelist/numerics_list/ dt, t_max

     open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
     call assert(io_status == success, "define(numerics,...): io_status == success", error_message)
     read(file_unit, nml=numerics_list)
     close(file_unit)

     new_numerics_t%dt_ = dt
     new_numerics_t%t_max_ = t_max
  end function

   pure function dt(this) result(this_dt)
     class(numerics_t), intent(in) :: this
     real(rkind) this_dt
     this_dt = this%dt_
   end function

   pure function t_max(this) result(this_t_max)
     class(numerics_t), intent(in) :: this
     real(rkind) this_t_max
     this_t_max = this%t_max_
   end function

end module numerics_module
