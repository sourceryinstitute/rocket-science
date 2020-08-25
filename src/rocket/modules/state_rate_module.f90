module state_rate_module
  !! Encapsulate the time rate of change of the state variables that are defined as
  !! state_t components.  These rate variables are used in the main program,
  !! where a state_rate_t object gets multiplied by the time step:
  !! motor%d_dt(state)*dt
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

  private
  public :: state_rate_t

  type state_rate_t
    private
    real(rkind) mass_rate_       !! chamber mass increase per unit time
    real(rkind) energy_rate_     !! chamber internal energy increase per unit time
    real(rkind) burn_depth_rate_ !! surface-normal burn depth increase per unit time
    real(rkind) time_rate_       !! time increase per unit time (always 1)
  contains
    procedure :: mass_rate
    procedure :: burn_depth_rate
    procedure :: energy_rate
    procedure :: multiply
    generic :: operator(*) => multiply
  end type

  interface state_rate_t
    module procedure new_rate
  end interface

contains

  pure function new_rate(time_rate, mass_rate, energy_rate, burn_depth_rate)
    !! result is a newly constructured state_rate_t object
    real(rkind), intent(in) :: time_rate, mass_rate, energy_rate, burn_depth_rate
    type(state_rate_t) new_rate

    new_rate%time_rate_ = time_rate
    new_rate%mass_rate_ = mass_rate
    new_rate%energy_rate_ = energy_rate
    new_rate%burn_depth_rate_ = burn_depth_rate
  end function

  pure function multiply(this, rhs) result(this_x_rhs)
    !! result has components computed from multiply each rhs component by dt
    use state_module, only : state_t
    class(state_rate_t), intent(in) :: this
    real(rkind), intent(in) :: rhs
    type(state_t) this_x_rhs

    this_x_rhs = state_t( &
      mass       = this%mass_rate_*rhs, &
      energy     = this%energy_rate_*rhs, &
      burn_depth = this%burn_depth_rate_*rhs, &
      time       = this%time_rate_*rhs &
    )
  end function

  pure function burn_depth_rate(this)
    class(state_rate_t), intent(in) :: this
    real(rkind) burn_depth_rate
    burn_depth_rate = this%burn_depth_rate_
  end function

  pure function energy_rate(this)
    class(state_rate_t), intent(in) :: this
    real(rkind) energy_rate
    energy_rate = this%energy_rate_
  end function

  pure function mass_rate(this)
    class(state_rate_t), intent(in) :: this
    real(rkind) mass_rate
    mass_rate = this%mass_rate_
  end function

end module
