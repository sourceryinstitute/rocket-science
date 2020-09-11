module gas_interface
  !! Encapsulate gas thermodynamic state and state relations
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind

  implicit none

  private
  public :: gas_t

  type gas_t
    !! encapsulate gas thermodynamic properties
    private
    real(rkind) c_p_, MW_
  contains
    procedure :: define !! read all gas_t components from a file
    procedure :: T      !! temperature
    procedure :: c_p    !! specific enthalpy
    procedure :: R_gas  !! gas constant
    procedure :: g      !! ratio of specific heat capacities
    procedure :: p      !! absolute pressure
    procedure :: c_v    !! specific heart at constant volume
  end type

  interface

    module subroutine define(this, input_file)
      !! Read gas components from input file
      implicit none
      class(gas_t), intent(out) :: this
      character(len=*), intent(in) :: input_file
    end subroutine

    pure module function T(this, energy, mass)
      !! Result is the gas temperature
      implicit none
      class(gas_t), intent(in) :: this
      real(rkind), intent(in) :: energy, mass
      real(rkind) T
    end function

    pure module function R_gas(this)
      !! Result is the gas constant
      implicit none
      class(gas_t), intent(in) :: this
      real(rkind) R_gas
    end function

    pure module function c_v(this)
      !! Result is the specific heat capacity at constant volume
      implicit none
      class(gas_t), intent(in) :: this
      real(rkind) c_v
    end function

    pure module function g(this)
      !! Result is the ratio of specific heat capacities
      implicit none
      class(gas_t), intent(in) :: this
      real(rkind) g
    end function

    pure module function c_p(this)
      !! Result is the specific enthalpy
      implicit none
      class(gas_t), intent(in) :: this
      real(rkind) c_p
    end function

    elemental module function p(this, energy, mass, volume)
      !! Result is the thermodynamic pressure
      implicit none
      class(gas_t), intent(in) :: this
      real(rkind), intent(in) :: energy, mass, volume
      real(rkind) p
    end function

  end interface

end module gas_interface
