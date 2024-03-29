module state_interface
  !! Encapsulate state variables that must persist throughught execution for purposes of accumulation
  use kind_parameters, only : rkind
  implicit none

  private

  type, public :: state_t
    private
    real(rkind) time_       !! simulated time
    real(rkind) mass_       !! mass contained in chamber
    real(rkind) energy_     !! internal energy contained in chamber
    real(rkind) burn_depth_ !! surface-normal burn distance
  contains
    procedure :: define, row_vector, energy, mass, burn_depth, time, absolute
    procedure, private :: add, multiply
    generic :: operator(+) => add
    generic :: operator(*) => multiply
  end type

  interface state_t
    module procedure new_state
  end interface

  interface

    module subroutine define(this, input_file, R_gas, c_v, volume, time, burn_depth)
      !! set all components of this gas_t object
      implicit none
      class(state_t), intent(out) :: this
      character(len=*), intent(in) :: input_file
      real(rkind), intent(in) :: R_gas, c_v, volume, time, burn_depth
    end subroutine
    
    pure module function absolute(this) result(this_abs)
      !! result is absolute value of the state vector
      implicit none
      class(state_t), intent(in) :: this
      real(rkind), allocatable :: this_abs(:)
    end function

    pure module function new_state(mass, energy, burn_depth, time)
      !! result is a new state_t object
      implicit none
      real(rkind), intent(in) ::  mass, energy, burn_depth, time
      type(state_t) new_state
    end function

    pure module function row_vector(this)
      implicit none
      class(state_t), intent(in) :: this
      real(rkind), allocatable :: row_vector(:,:)
    end function

    elemental module function time(this)
      !! get the time state variable
      implicit none
      class(state_t), intent(in) :: this
      real(rkind) time
    end function

    elemental module function burn_depth(this)
      !! get the burn_depth state variable
      implicit none
      class(state_t), intent(in) :: this
      real(rkind) burn_depth
    end function

    elemental module function energy(this)
      !! get the energy state variable
      implicit none
      class(state_t), intent(in) :: this
      real(rkind) energy
    end function

    elemental module function mass(this)
      !! get the mass state variable
      implicit none
      class(state_t), intent(in) :: this
      real(rkind) mass
    end function

    pure module function add(lhs, rhs) result(total)
      !! result has components computed from summing lhs & rhs components
      implicit none
      class(state_t), intent(in) :: lhs, rhs
      type(state_t) total
    end function

    pure module function multiply(lhs, rhs) result(product_)
      !! result has components computed from summing lhs & rhs components
      implicit none
      class(state_t), intent(in) :: lhs
      real(rkind), intent(in) :: rhs
      type(state_t) product_
    end function

  end interface

end module state_interface
