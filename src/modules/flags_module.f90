module flags_module
  !! This module encapsulates flags.
  use kind_parameters, only : DP

  implicit none

  private
  public :: flags
  public :: initialize
  public :: get_temp
  public :: get_dt
  public :: get_tmax
  public :: set_temp
  public :: set_dt
  public :: set_tmax

  type flags
    private
    real(DP) :: temp, dt, tmax
  end type

contains

  subroutine initialize(this, input_file)
    type(flags), intent(out) :: this
    character(len=*), intent(in) :: input_file
    integer u
    real(DP) :: dt, tmax

    namelist /time/ dt, tmax

    open(newunit=u, file=input_file, status='old')
    read(u, nml=time)
    close(u)

    this%dt = dt
    this%tmax = tmax

  end subroutine

  function get_temp(this) result(this_temp)
    type(flags), intent(in) :: this
    real(DP) :: this_temp
    this_temp = this%temp
  end function

  function get_dt(this) result(this_dt)
    type(flags), intent(in) :: this
    real(DP) :: this_dt
    this_dt = this%dt
  end function

  function get_tmax(this) result(this_tmax)
    type(flags), intent(in) :: this
    real(DP) :: this_tmax
    this_tmax = this%tmax
  end function

  subroutine set_temp(this, temp)
    type(flags), intent(inout) :: this
    real(DP), intent(in) :: temp
    this%temp = temp
  end subroutine

  subroutine set_dt(this, dt)
    type(flags), intent(inout) :: this
    real(DP), intent(in) :: dt
    this%dt = dt
  end subroutine

  subroutine set_tmax(this, tmax)
    type(flags), intent(inout) :: this
    real(DP), intent(in) :: tmax
    this%tmax = tmax
  end subroutine

end module flags_module
