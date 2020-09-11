submodule(state_rate_interface) state_rate_implementation
  use kind_parameters, only : rkind
  implicit none

contains

  module procedure new_rate
    new_rate%time_rate_ = time_rate
    new_rate%mass_rate_ = mass_rate
    new_rate%energy_rate_ = energy_rate
    new_rate%burn_depth_rate_ = burn_depth_rate
  end procedure

  module procedure multiply
    use state_interface, only : state_t

    this_x_rhs = state_t( &
      mass       = this%mass_rate_*rhs, &
      energy     = this%energy_rate_*rhs, &
      burn_depth = this%burn_depth_rate_*rhs, &
      time       = this%time_rate_*rhs &
    )
  end procedure

end submodule state_rate_implementation
