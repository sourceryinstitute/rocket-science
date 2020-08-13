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
    procedure :: define
    procedure :: dt
    procedure :: t_max
  end type

contains

  subroutine define(this, input_file)
     use assertions_interface, only : assert, max_errmsg_len
     class(numerics_t), intent(out) :: this
     character(len=*), intent(in) :: input_file
     real(rkind) dt, t_max
     namelist/numerics_list/ dt, t_max

     block
       character(len=max_errmsg_len) error_message
       integer io_status, file_unit
       integer, parameter :: success =0

       open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
       call assert(io_status == success, "numerics_t%define: io_status == success", error_message)
       read(file_unit, nml=numerics_list)
       close(file_unit)
     end block

     this%dt_ = dt
     this%t_max_ = t_max
  end subroutine

   pure function dt(this)
     class(numerics_t), intent(in) :: this
     real(rkind) dt
     dt = this%dt_
   end function

   pure function t_max(this)
     class(numerics_t), intent(in) :: this
     real(rkind) t_max
     t_max = this%t_max_
   end function

end module numerics_module
