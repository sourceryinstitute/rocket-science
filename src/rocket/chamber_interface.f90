module chamber_interface
  !! Encapsulate the chamber components: propellant grain, combustion model, gas, & nozzle geometry
  use assertions_interface, only : assert, max_errmsg_len
  use gas_interface, only : gas_t
  use combustion_interface, only : combustion_t
  use nozzle_interface, only : nozzle_t
  use grain_interface, only : grain_t
  use kind_parameters, only : rkind
  implicit none

  private

  type, public :: chamber_t
    private
    type(grain_t) grain_
    type(gas_t) gas_
    type(combustion_t) combustion_
    type(nozzle_t) nozzle_
  contains
    procedure :: define
    procedure :: gas
    procedure :: thrust
    procedure :: pressure
    procedure :: volume
    procedure :: temperature
    procedure :: initial_volume
    procedure :: outflow
    procedure :: generate
    procedure :: burn_rate
  end type

  real(rkind), parameter :: T_ambient=300._rkind, p_ambient=101325._rkind

  interface

    module subroutine define(this, input_file)
      !! Set all chamber components
      implicit none
      class(chamber_t), intent(out) ::  this
      character(len=*), intent(in) :: input_file
    end subroutine

    pure module function gas(this)
      implicit none
      class(chamber_t), intent(in) :: this
      type(gas_t) gas
    end function

    elemental module function volume(this, burn_depth)
      implicit none
      class(chamber_t), intent(in) :: this
      real(rkind), intent(in) :: burn_depth
      real(rkind) volume
    end function

    elemental module function temperature(this, energy, mass)
      implicit none
      class(chamber_t), intent(in) :: this
      real(rkind), intent(in) :: energy, mass
      real(rkind) temperature
    end function

    elemental module function thrust(this, pressure)
      implicit none
      class(chamber_t), intent(in) :: this
      real(rkind), intent(in) :: pressure
      real(rkind) thrust
    end function

    pure module function initial_volume(this)
      implicit none
      class(chamber_t), intent(in) :: this
      real(rkind) initial_volume
    end function

    pure module function burn_rate(this, state)
      !! Result is the rate of surface-normal depth loss for the burning tablets
      use state_interface, only : state_t
      implicit none
      class(chamber_t), intent(in) :: this
      type(state_t), intent(in) :: state
      real(rkind) burn_rate
    end function

    pure module function generate(this, state) result(rate)
      !! Result contains the burn rate, mass generation rate, and energy generation rate
      use state_interface, only : state_t
      use generation_rate_interface, only : generation_rate_t
      implicit none
      class(chamber_t), intent(in) :: this
      type(state_t), intent(in) :: state
      type(generation_rate_t) rate
    end function

    elemental module function outflow(this, state) result(rate)
      use flow_rate_interface, only : flow_rate_t
      use state_interface, only : state_t
      implicit none
      class(chamber_t), intent(in) :: this
      type(state_t), intent(in) :: state
      type(flow_rate_t) rate
    end function

    elemental module function pressure(this, energy, mass, volume)
      implicit none
      class(chamber_t), intent(in) :: this
      real(rkind), intent(in) :: energy, mass, volume
      real(rkind) pressure
    end function

  end interface

end module chamber_interface
