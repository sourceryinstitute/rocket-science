module flags_module
  !! This module encapsulates flags.
  use kind_parameters, only : rkind

  implicit none

  private
  public :: flags
  public :: initialize
  public :: get_dt
  public :: get_tmax
  public :: set_dt
  public :: set_tmax

  type flags
    private
    real(rkind) :: dt, tmax
  end type

contains

  subroutine initialize(this, input_file)
    type(flags), intent(out) :: this
    character(len=*), intent(in) :: input_file
    integer u
    real(rkind) :: dt, tmax

    namelist /time/ dt, tmax

    open(newunit=u, file=input_file, status='old')
    read(u, nml=time)
    close(u)

    this%dt = dt
    this%tmax = tmax

  end subroutine

  function get_dt(this) result(this_dt)
    type(flags), intent(in) :: this
    real(rkind) :: this_dt
    this_dt = this%dt
  end function

  function get_tmax(this) result(this_tmax)
    type(flags), intent(in) :: this
    real(rkind) :: this_tmax
    this_tmax = this%tmax
  end function

  subroutine set_dt(this, dt)
    type(flags), intent(inout) :: this
    real(rkind), intent(in) :: dt
    this%dt = dt
  end subroutine

  subroutine set_tmax(this, tmax)
    type(flags), intent(inout) :: this
    real(rkind), intent(in) :: tmax
    this%tmax = tmax
  end subroutine

end module flags_module
