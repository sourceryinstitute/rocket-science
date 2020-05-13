module flags_module
  !! This module encapsulates flags.
  use kind_parameters, only : DP

  implicit none

  private
  public :: flags
  public :: get_dt

  type flags
    private
    real(DP) :: temp_, dt_, tmax_
  end type

contains

  function get_temp(this) result(this_temp)
    type(flags), intent(in) :: this
    real(DP) :: this_temp
    this_temp = this%temp_
  end function

  function get_dt(this) result(this_dt)
    type(flags), intent(in) :: this
    real(DP) :: this_dt
    this_dt = this%dt_
  end function

  function get_tmax(this) result(this_tmax)
    type(flags), intent(in) :: this
    real(DP) :: this_tmax
    this_tmax = this%tmax_
  end function

  subroutine set_temp(this, temp)
    type(flags), intent(inout) :: this
    real(DP), intent(in) :: temp
    this%temp_ = temp
  end subroutine

  subroutine set_dt(this, dt)
    type(flags), intent(inout) :: this
    real(DP), intent(in) :: dt
    this%dt_ = dt
  end subroutine

  subroutine set_tmax(this, tmax)
    type(flags), intent(inout) :: this
    real(DP), intent(in) :: tmax
    this%tmax_ = tmax
  end subroutine

end module flags_module
