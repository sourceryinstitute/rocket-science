module persistent_state_module
  !! Encapsulate state variables that must persist throughught execution for purposes of accumulation
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

  private
  public :: persistent_state_t
  public :: define
  public :: set_time
  public :: set_burn_depth
  public :: set_mass
  public :: set_energy
  public :: time
  public :: burn_depth
  public :: operator(+)
  public :: operator(*)
  public :: output

  type persistent_state_t
    private
    real(rkind) mass       !! mass contained in chamber
    real(rkind) energy     !! internal energy contained in chamber
    real(rkind) time       !! simulated time
    real(rkind) burn_depth !! surface-normal burn distance
  end type

  interface define
    module procedure define_persistent_state
  end interface

  interface output
    module procedure output_persistent_state
  end interface

  interface operator(*)
    module procedure multiply
  end interface

  interface operator(+)
    module procedure add
  end interface

contains

  subroutine define_persistent_state(this, mass, energy, time, burn_depth)
    !! (re)set the entire object state
    type(persistent_state_t), intent(out) :: this
    real(rkind), intent(in) :: mass
    real(rkind), intent(in) :: energy
    real(rkind), intent(in) :: time
    real(rkind), intent(in) :: burn_depth
    this%time = time
    this%burn_depth = burn_depth
    this%mass = mass
    this%energy = energy
  end subroutine

  subroutine output_persistent_state(this, file_unit)
    type(persistent_state_t), intent(in) :: this
    integer, intent(in) :: file_unit
    write(file_unit,*) this%time, this%mass, this%energy, this%burn_depth
  end subroutine

  subroutine set_time(this, time)
    !! (re)set the time state variable
    type(persistent_state_t), intent(inout) :: this
    real(rkind), intent(in) :: time
    this%time = time
  end subroutine

  subroutine set_burn_depth(this, burn_depth)
    !! (re)set the burn_depth state variable
    real(rkind), intent(in) :: burn_depth
    type(persistent_state_t), intent(inout) :: this
    this%burn_depth = burn_depth
  end subroutine

  subroutine set_mass(this, mass)
    !! (re)set the mass state variable
    real(rkind), intent(in) :: mass
    type(persistent_state_t), intent(inout) :: this
    this%mass = mass
  end subroutine

  subroutine set_energy(this, energy)
    !! (re)set the energy state variable
    real(rkind), intent(in) :: energy
    type(persistent_state_t), intent(inout) :: this
    this%energy = energy
  end subroutine

  function time(this) result(this_time)
    !! get the time state variable
    type(persistent_state_t), intent(in) :: this
    real(rkind) this_time
    this_time = this%time
  end function

  function burn_depth(this) result(this_burn_depth)
    !! get the burn_depth state variable
    type(persistent_state_t), intent(in) :: this
    real(rkind) this_burn_depth
    this_burn_depth = this%burn_depth
  end function

  function add(lhs, rhs) result(total)
    !! result has components computed from summing lhs & rhs components
    type(persistent_state_t), intent(in) :: lhs, rhs
    type(persistent_state_t) total
    total%time       = lhs%time       + rhs%time
    total%burn_depth = lhs%burn_depth + rhs%burn_depth
    total%mass       = lhs%mass       + rhs%mass
    total%energy     = lhs%energy    + rhs%energy
  end function

  function multiply(lhs, rhs) result(product)
    !! result has components computed from multiply each rhs component by lhs
    real(rkind), intent(in) :: lhs
    type(persistent_state_t), intent(in) :: rhs
    type(persistent_state_t) product
    product%time       = lhs*rhs%time
    product%burn_depth = lhs*rhs%burn_depth
    product%mass       = lhs*rhs%mass
    product%energy     = lhs*rhs%energy
  end function

end module
