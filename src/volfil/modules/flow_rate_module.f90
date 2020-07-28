module flow_rate_module
  !! Encapsulate mass and energy flow rates to facilitate functional programming:
  !! because the energy flow rate depends on the mass flow rate, calculating both
  !! in one function avoids redundancy, functions can have only one result so a
  !! flow_rate_t object captures both results in one object.
  use kind_parameters, only : DP
  implicit none

  private
  public :: flow_rate_t
  public :: e_dot_out
  public :: m_dot_out

  type flow_rate_t
    private
    real(DP) e_dot_o
    real(DP) m_dot_o
  end type

  interface flow_rate_t
    module procedure define_flow_rate
  end interface

contains

  function define_flow_rate(mass_outflow_rate, energy_outflow_rate) result(rate)
    real(DP), intent(in) :: mass_outflow_rate, energy_outflow_rate
    type(flow_rate_t) rate
    rate%m_dot_o = mass_outflow_rate
    rate%e_dot_o = energy_outflow_rate
  end function

  function m_dot_out(this) result(m_dot)
    type(flow_rate_t), intent(in) :: this
    real(DP) m_dot
    m_dot = this%m_dot_o
  end function

  function e_dot_out(this) result(e_dot)
    type(flow_rate_t), intent(in) :: this
    real(DP) e_dot
    e_dot = this%e_dot_o
  end function

end module flow_rate_module
