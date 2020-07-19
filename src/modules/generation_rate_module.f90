module generation_rate_module
  !! Encapsulate mass and energy generation rates to facilitate functional programming:
  !! because the energy generation rate depends on the mass generation rate, calculating
  !! both in one function avoids redundancy. Functions can have only one result so a
  !! generation_rate_t object captures both results in one object.
  use kind_parameters, only : DP
  implicit none

  private
  public :: generation_rate_t
  public :: define
  public :: burn_rate
  public :: e_dot_gen
  public :: m_dot_gen

  type generation_rate_t
    private
    real(DP) burn_rate
    real(DP) e_dot_gen
    real(DP) m_dot_gen
  end type

  interface define
    module procedure define_generation_rate
  end interface

contains

  subroutine define_generation_rate(this, burn_rate, mass_generation_rate, energy_generation_rate)
    type(generation_rate_t), intent(out) :: this
    real(DP), intent(in) :: burn_rate, mass_generation_rate, energy_generation_rate
    this%burn_rate = burn_rate
    this%m_dot_gen = mass_generation_rate
    this%e_dot_gen = energy_generation_rate
  end subroutine

  function burn_rate(this) result(rate)
    type(generation_rate_t), intent(in) :: this
    real(DP) rate
    rate = this%burn_rate
  end function

  function m_dot_gen(this) result(m_dot)
    type(generation_rate_t), intent(in) :: this
    real(DP) m_dot
    m_dot = this%m_dot_gen
  end function

  function e_dot_gen(this) result(e_dot)
    type(generation_rate_t), intent(in) :: this
    real(DP) e_dot
    e_dot = this%e_dot_gen
  end function

end module generation_rate_module
