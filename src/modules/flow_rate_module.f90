module flow_rate_module
  use kind_parameters, only : DP
  implicit none

  private
  public :: flow_rate_t
  public :: define
  public :: e_dot_out
  public :: m_dot_out

  type flow_rate_t
    private
    real(DP) e_dot_o
    real(DP) m_dot_o
  end type

  interface define
    module procedure define_flow_rate
  end interface

contains

  subroutine define_flow_rate(this, mass_outflow_rate, energy_outflow_rate)
    type(flow_rate_t), intent(out) :: this
    real(DP), intent(in) :: mass_outflow_rate, energy_outflow_rate
    this%m_dot_o = mass_outflow_rate
    this%e_dot_o = energy_outflow_rate
  end subroutine

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
