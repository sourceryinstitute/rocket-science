module flow_rate_module
  !! Encapsulate mass and energy flow rates to facilitate functional programming:
  !! because the energy flow rate depends on the mass flow rate, calculating both
  !! in one function avoids redundancy, functions can have only one result so a
  !! flow_rate_t object captures both results in one object.
  use kind_parameters, only : rkind
  implicit none

  private
  public :: flow_rate_t

  type flow_rate_t
    private
    real(rkind) e_dot_out_
    real(rkind) m_dot_out_
  contains
    procedure :: e_dot_out
    procedure :: m_dot_out
  end type

  interface flow_rate_t
    module procedure new_flow_rate
  end interface

contains

  pure function new_flow_rate(mass_outflow_rate, energy_outflow_rate)
    real(rkind), intent(in) :: mass_outflow_rate, energy_outflow_rate
    type(flow_rate_t) new_flow_rate
    new_flow_rate%m_dot_out_ = mass_outflow_rate
    new_flow_rate%e_dot_out_ = energy_outflow_rate
  end function

  pure function m_dot_out(this)
    class(flow_rate_t), intent(in) :: this
    real(rkind) m_dot_out
    m_dot_out = this%m_dot_out_
  end function

  pure function e_dot_out(this)
    class(flow_rate_t), intent(in) :: this
    real(rkind) e_dot_out
    e_dot_out = this%e_dot_out_
  end function

end module flow_rate_module
