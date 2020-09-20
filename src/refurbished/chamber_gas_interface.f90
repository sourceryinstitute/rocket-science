module chamber_gas_interface
  !! Model the thermodynamic state of the liberated gas in the motor chamber
  use constants, only : dp
  implicit none

  private

  type, public :: chamber_gas_t
    !! Encapsulate the thermodynamic state of the chamber gas
    private
    real(dp) MW_    !! gas molecular weight
    real(dp) c_p_   !! constant-pressure specific heat
    real(dp) mcham_ !! mass
    real(dp) echam_ !! internal energy
  contains
    procedure :: R_gas      !! ideal gas constant
    procedure :: c_p        !! constant-pressure specific heat capacity
    procedure :: c_v        !! constant-volume specific heat capacity
    procedure :: T          !! temperature
    procedure :: p => calcp !! pressure
    procedure :: g          !! ratio of specific heats (gamma = c_p/c_v)
  end type

  interface chamber_gas_t !! Generic interface for user-defined structure constructors
    module procedure new_chamber_gas_t, incremented_chamber_gas_t
  end interface

  interface

    pure module function new_chamber_gas_t(MW, c_p, T, p, V)
      !! Result is a new chamber_gas_t object defined by the dummy arguments
      implicit none
      real(dp), intent(in) :: MW, c_p, T, p, V
      type(chamber_gas_t) new_chamber_gas_t
    end function

    pure module function incremented_chamber_gas_t(old_chamber_gas_t, mass_increment, energy_increment)
      !! Result is a new chamber_gas_t object defined as an update to the old_chamber_gas_t object
      !! by incrementing the mass and energy of the old_chamber_gas_t
      implicit none
      type(chamber_gas_t), intent(in) :: old_chamber_gas_t
      real(dp), intent(in) :: mass_increment, energy_increment
      type(chamber_gas_t) incremented_chamber_gas_t
    end function

    pure module function R_gas(this)
      !! Result is the ideal gas constant
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) R_gas
    end function

    pure module function c_p(this)
      !! Result is the constant-pressure specific heat capacity
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) c_p
    end function

    pure module function c_v(this)
      !! Result is the constant-volume specific heat capacity
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) c_v
    end function

    pure module function T(this)
      !! Result is the temperature
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) T
    end function

    pure module function g(this)
      !! Result is the ratio of specific heat capacities (c_p/c_v)
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) g
    end function

    pure module function calcp(this, V)
      !! Result is the thermodynamic pressure
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp), intent(in) :: V
      real(dp) :: calcp
    end function

  end interface

end module chamber_gas_interface
