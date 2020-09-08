module flow_rate_interface
  !! Encapsulate mass and energy flow rates to facilitate functional programming:
  !! because the energy flow rate depends on the mass flow rate, calculating both
  !! in one function avoids redundancy, but functions can have only one result so a
  !! flow_rate_t object captures both results in one object.
  use kind_parameters, only : rkind
  implicit none

  private

  type, public :: flow_rate_t
    private
    real(rkind) e_dot_ !! energy flow rate
    real(rkind) m_dot_ !! mass flow rate
  contains
    procedure :: e_dot, m_dot
  end type

  interface flow_rate_t
    module procedure new_flow_rate
  end interface

  interface

    pure module function new_flow_rate(mass_flow_rate, energy_flow_rate)
      implicit none
      real(rkind), intent(in) :: mass_flow_rate, energy_flow_rate
      type(flow_rate_t) new_flow_rate
    end function

    pure module function m_dot(this)
      implicit none
      class(flow_rate_t), intent(in) :: this
      real(rkind) m_dot
    end function

    pure module function e_dot(this)
      implicit none
      class(flow_rate_t), intent(in) :: this
      real(rkind) e_dot
    end function

  end interface

end module flow_rate_interface
