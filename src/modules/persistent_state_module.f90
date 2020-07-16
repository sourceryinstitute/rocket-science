module persistent_state_module
  !! Encapsulate state variables that must persist throughught execution for purposes of accumulation
  use kind_parameters, only : DP
  implicit none

  private
  public :: persistent_state_t
  public :: define
  public :: set_time
  public :: set_burn_depth
  public :: time
  public :: burn_depth

  type persistent_state_t
    private
    real(DP) time
    real(DP) burn_depth !! surface-normal burn distance
  end type

  interface define
    module procedure define_persistent_state
  end interface

contains

  subroutine define_persistent_state(this, time, burn_depth)
    !! (re)set the entire object state
    type(persistent_state_t), intent(out) :: this
    real(DP), intent(in) :: time
    real(DP), intent(in) :: burn_depth
  end subroutine

  subroutine set_time(this, time)
    !! (re)set the time state variable
    type(persistent_state_t), intent(inout) :: this
    real(DP), intent(in) :: time
    this%time = time
  end subroutine

  subroutine set_burn_depth(this, burn_depth)
    !! (re)set the burn_depth state variable
    real(DP), intent(in) :: burn_depth
    type(persistent_state_t), intent(inout) :: this
    this%burn_depth = burn_depth
  end subroutine

  function time(this) result(this_time)
    !! get the time state variable
    type(persistent_state_t), intent(in) :: this
    real(DP) this_time
    this_time = this%time
  end function

  function burn_depth(this) result(this_burn_depth)
    !! get the burn_depth state variable
    type(persistent_state_t), intent(in) :: this
    real(DP) this_burn_depth
    this_burn_depth = this%burn_depth
  end function

end module
