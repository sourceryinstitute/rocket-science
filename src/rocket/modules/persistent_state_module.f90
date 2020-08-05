module persistent_state_module
  !! Encapsulate state variables that must persist throughught execution for purposes of accumulation
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

  private
  public :: persistent_state_t

  type persistent_state_t
    private
    real(rkind) mass_       !! mass contained in chamber
    real(rkind) energy_     !! internal energy contained in chamber
    real(rkind) time_       !! simulated time
    real(rkind) burn_depth_ !! surface-normal burn distance
  contains
    procedure :: set_time
    procedure :: set_burn_depth
    procedure :: set_mass
    procedure :: set_energy
    procedure :: time
    procedure :: burn_depth
    procedure, pass(rhs) :: multiply
    procedure :: add
    procedure :: output
    generic :: operator(+) => add
    generic :: operator(*) => multiply
  end type

  interface persistent_state_t
    module procedure construct_persistent_state_t
  end interface

contains

  pure function construct_persistent_state_t(mass, energy, time, burn_depth) result(new_persistent_state_t)
    !! result is a newly constructured persistent_state_t object
    real(rkind), intent(in) :: mass
    real(rkind), intent(in) :: energy
    real(rkind), intent(in) :: time
    real(rkind), intent(in) :: burn_depth
    type(persistent_state_t) new_persistent_state_t

    new_persistent_state_t%time_ = time
    new_persistent_state_t%burn_depth_ = burn_depth
    new_persistent_state_t%mass_ = mass
    new_persistent_state_t%energy_ = energy
  end function

  subroutine output(this, file_unit)
    class(persistent_state_t), intent(in) :: this
    integer, intent(in) :: file_unit
    write(file_unit,*) this%time_, this%mass_, this%energy_, this%burn_depth_
  end subroutine

  pure subroutine set_time(this, time)
    !! (re)set the time state variable
    class(persistent_state_t), intent(inout) :: this
    real(rkind), intent(in) :: time
    this%time_ = time
  end subroutine

  pure subroutine set_burn_depth(this, burn_depth)
    !! (re)set the burn_depth state variable
    real(rkind), intent(in) :: burn_depth
    class(persistent_state_t), intent(inout) :: this
    this%burn_depth_ = burn_depth
  end subroutine

  pure subroutine set_mass(this, mass)
    !! (re)set the mass state variable
    real(rkind), intent(in) :: mass
    class(persistent_state_t), intent(inout) :: this
    this%mass_ = mass
  end subroutine

  pure subroutine set_energy(this, energy)
    !! (re)set the energy state variable
    real(rkind), intent(in) :: energy
    class(persistent_state_t), intent(inout) :: this
    this%energy_ = energy
  end subroutine

  pure function time(this) result(this_time)
    !! get the time state variable
    class(persistent_state_t), intent(in) :: this
    real(rkind) this_time
    this_time = this%time_
  end function

  pure function burn_depth(this) result(this_burn_depth)
    !! get the burn_depth state variable
    class(persistent_state_t), intent(in) :: this
    real(rkind) this_burn_depth
    this_burn_depth = this%burn_depth_
  end function

  pure function add(lhs, rhs) result(total)
    !! result has components computed from summing lhs & rhs components
    class(persistent_state_t), intent(in) :: lhs, rhs
    type(persistent_state_t) total
    total%time_       = lhs%time_       + rhs%time_
    total%burn_depth_ = lhs%burn_depth_ + rhs%burn_depth_
    total%mass_       = lhs%mass_       + rhs%mass_
    total%energy_     = lhs%energy_    + rhs%energy_
  end function

  pure function multiply(lhs, rhs) result(product)
    !! result has components computed from multiply each rhs component by lhs
    class(persistent_state_t), intent(in) :: rhs
    real(rkind), intent(in) :: lhs
    type(persistent_state_t) product
    product%time_       = lhs*rhs%time_
    product%burn_depth_ = lhs*rhs%burn_depth_
    product%mass_       = lhs*rhs%mass_
    product%energy_     = lhs*rhs%energy_
  end function

end module
