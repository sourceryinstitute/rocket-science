module generation_rate_module
  !! Encapsulate mass and energy generation rates to facilitate functional programming:
  !! because the energy generation rate depends on the mass generation rate, calculating
  !! both in one function avoids redundancy. Functions can have only one result so a
  !! generation_rate_t object captures both results in one object.
  use kind_parameters, only : rkind
  implicit none

  private
  public :: generation_rate_t

  type generation_rate_t
    private
    real(rkind) burn_rate_
    real(rkind) e_dot_gen_
    real(rkind) m_dot_gen_
  contains
    procedure :: burn_rate
    procedure :: e_dot_gen
    procedure :: m_dot_gen
  end type

  interface generation_rate_t
    module procedure new_generation_rate
  end interface

contains

  pure function new_generation_rate(burn_rate, mass_generation_rate, enthalpy_generation_rate)
    real(rkind), intent(in) :: burn_rate, mass_generation_rate, enthalpy_generation_rate
    type(generation_rate_t) new_generation_rate
    new_generation_rate%burn_rate_= burn_rate
    new_generation_rate%m_dot_gen_= mass_generation_rate
    new_generation_rate%e_dot_gen_= enthalpy_generation_rate
  end function

  pure function burn_rate(this)
    class(generation_rate_t), intent(in) :: this
    real(rkind) burn_rate
    burn_rate = this%burn_rate_
  end function

  pure function m_dot_gen(this)
    class(generation_rate_t), intent(in) :: this
    real(rkind) m_dot_gen
    m_dot_gen = this%m_dot_gen_
  end function

  pure function e_dot_gen(this)
    class(generation_rate_t), intent(in) :: this
    real(rkind) e_dot_gen
    e_dot_gen = this%e_dot_gen_
  end function

end module generation_rate_module
