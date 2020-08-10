module state_rate_module
  !! Encapsulate state variables that must persist throughught execution for purposes of accumulation
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

  private
  public :: state_rate_t

  type state_rate_t
    private
    real(rkind) time_rate_       !! time increase per unit time (always 1)
    real(rkind) mass_rate_       !! chamber mass increase per unit time
    real(rkind) energy_rate_     !! chamber internal energy increase per unit time
    real(rkind) burn_depth_rate_ !! surface-normal burn depth increase per unit time
  contains
    procedure :: mass_rate
    procedure :: burn_depth_rate
    procedure :: energy_rate
    procedure :: multiply
    generic :: operator(*) => multiply
  end type

  interface state_rate_t
    module procedure construct
  end interface

contains

  pure function construct(time_rate, mass_rate, energy_rate, burn_depth_rate) result(new_rate)
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
    use persistent_state_module, only : persistent_state_t
    class(state_rate_t), intent(in) :: this
    real(rkind), intent(in) :: rhs
    type(persistent_state_t) this_x_rhs

    this_x_rhs = persistent_state_t( &
      mass       = this%mass_rate_*rhs, &
      energy     = this%energy_rate_*rhs, &
      burn_depth = this%burn_depth_rate_*rhs, &
      time       = this%time_rate_*rhs &
    )
  end function

  pure function burn_depth_rate(this) result(this_burn_depth_rate)
    class(state_rate_t), intent(in) :: this
    real(rkind) this_burn_depth_rate
    this_burn_depth_rate = this%burn_depth_rate_
  end function

  pure function energy_rate(this) result(this_energy)
    class(state_rate_t), intent(in) :: this
    real(rkind) this_energy
    this_energy = this%mass_rate_
  end function

  pure function mass_rate(this) result(this_mass_rate)
    class(state_rate_t), intent(in) :: this
    real(rkind) this_mass_rate
    this_mass_rate = this%mass_rate_
  end function

end module
