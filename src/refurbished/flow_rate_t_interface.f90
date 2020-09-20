module flow_rate_t_interface
  !! Describe the gas flow rates resulting from the chamber-pressure/exit-pressure differential
  use constants, only : dp
  implicit none

  private

  type, public :: flow_rate_t
    !! Encapsulate the gas mass and internal energy flow rates
    private
    real(dp) mdotos_ !! mass flow rate
    real(dp) edotos_ !! internal energy flow rate
  contains
    procedure :: mdotos
    procedure :: edotos
  end type

  interface flow_rate_t !! Generic interface to user-defined structure constructors
    module procedure new_flow_rate_t
  end interface

  interface

    pure module function new_flow_rate_t(t, g, rgas, p, cp, area) result(new_flow_rate)
       !! Result is a new flow_rate_t object defined by the actual arguments
       implicit none
       real(dp), intent(in) :: t       !! temperature
       real(dp), intent(in) :: g       !! ratio of specific heats
       real(dp), intent(in) :: rgas    !! ideal gas constant
       real(dp), intent(in) :: p       !! pressure
       real(dp), intent(in) :: cp      !! constant-pressure specific heat
       real(dp), intent(in) :: area    !! flow area
       type(flow_rate_t) new_flow_rate
    end function

    pure module function mdotos(this)
      !! Result is this flow_rate_t object's mass flow rate
      implicit none
      class(flow_rate_t), intent(in) :: this
      real(dp) mdotos
    end function

    pure module function edotos(this)
      !! Result is this flow_rate_t object's internal energy flow rate
      implicit none
      class(flow_rate_t), intent(in) :: this
      real(dp) edotos
    end function

  end interface

end module flow_rate_t_interface
