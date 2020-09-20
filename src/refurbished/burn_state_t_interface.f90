module burn_state_t_interface
  !! Capture the state and state-update methods of the propellent burning process
  use constants, only : dp
  implicit none

  private

  type, public :: burn_state_t
    !! Encapsulate the propellent grain burn rate and surface-normal burn depth
    private
    real(dp) r_      !! rate of burn-depth change per unit time
    real(dp) r_ref_  !! reference rate of burn-depth change per unit time
    real(dp) db_     !! surface-normal depth of propellent burned
  contains
    procedure :: db
    procedure :: r
  end type

  interface burn_state_t
    module procedure new_burn_state_t, zero_burn_depth
  end interface

  interface

    pure module function new_burn_state_t(old_burn_state_t, r_ref, p, n, dt)
      !! Result is a new burn_state_t object defined as an update to an old burn_state_t object
      implicit none
      type(burn_state_t) :: new_burn_state_t
      type(burn_state_t), intent(in) :: old_burn_state_t
      real(dp), intent(in) :: r_ref !! reference burn rate
      real(dp), intent(in) :: p     !! pressure
      real(dp), intent(in) :: n     !! burn-rate exponent
      real(dp), intent(in) :: dt    !! burn time
    end function

    pure module function zero_burn_depth(reference_burn_rate, pressure, exponent_)
      !! Set the burn-depth to zero and use the arguments to define the burn rate
      implicit none
      real(dp), intent(in) :: reference_burn_rate, pressure, exponent_
      type(burn_state_t) zero_burn_depth
    end function

    pure module function db(this)
      !! Result is the present burn depth
      implicit none
      class(burn_state_t), intent(in) :: this
      real(dp) db
    end function

    pure module function r(this)
      !! Result is the present burn rate
      implicit none
      class(burn_state_t), intent(in) :: this
      real(dp) r
    end function

  end interface

end module burn_state_t_interface
