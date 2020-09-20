submodule(burn_state_interface) burn_state_implementation
  !! Define the burn_state_t user-defined structure constructors and type-bound procedures
  implicit none

  real(dp), parameter :: psipa=6894.76d0    ! unit conversion factor: pascals per psi
  real(dp), parameter :: p_ref=3000d0*psipa ! constant reference pressure for burn-rate calculation

contains

  module procedure new_burn_state_t
    associate(r_ref => (old_burn_state_t%r_ref_))
      new_burn_state_t%r_ref_ = r_ref
      new_burn_state_t%r_ = r_ref*(p/p_ref)**n ! calculate burn rate
    end associate
    associate(r => (new_burn_state_t%r_))
      new_burn_state_t%db_ = old_burn_state_t%db_ + r*dt  ! increment burn depth
    end associate
  end procedure

  module procedure zero_burn_depth
    use constants, only : zero
    zero_burn_depth%r_ref_  = reference_burn_rate
    zero_burn_depth%r_      = reference_burn_rate*(pressure/p_ref)**exponent_
    zero_burn_depth%db_     = zero
  end procedure

  module procedure db
    db = this%db_
  end procedure

  module procedure r
    r = this%r_
  end procedure

end submodule burn_state_implementation
