module generation_rate_module
  use kind_parameters, only : DP
  implicit none

  private
  public :: generation_rate_t
  public :: define
  public :: delta_surface_normal
  public :: e_dot_gen
  public :: m_dot_gen

  type generation_rate_t
    private
    real(DP) delta_sn !! change in surface-normal depth
    real(DP) e_dot_g
    real(DP) m_dot_g
  end type

  interface define
    module procedure define_generation_rate
  end interface

contains

  subroutine define_generation_rate(this, mass_generation_rate, energy_generation_rate)
    type(generation_rate_t), intent(out) :: this
    real(DP), intent(in) :: mass_generation_rate, energy_generation_rate
    this%m_dot_g = mass_generation_rate
    this%e_dot_g = energy_generation_rate
  end subroutine

  function delta_surface_normal(this) result(delta)
    type(generation_rate_t), intent(in) :: this
    real(DP) delta
    delta = this%delta_sn
  end function

  function m_dot_gen(this) result(m_dot)
    type(generation_rate_t), intent(in) :: this
    real(DP) m_dot
    m_dot = this%m_dot_g
  end function

  function e_dot_gen(this) result(e_dot)
    type(generation_rate_t), intent(in) :: this
    real(DP) e_dot
    e_dot = this%e_dot_g
  end function

end module generation_rate_module
