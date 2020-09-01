module generation_rate_interface
  !! Encapsulate mass and energy generation rates to facilitate functional programming:
  !! because the energy generation rate depends on the mass generation rate, calculating
  !! both in one function avoids redundancy. Functions can have only one result so a
  !! generation_rate_t object captures both results in one object.
  use kind_parameters, only : rkind
  implicit none

  private

  type, public :: generation_rate_t
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

  interface

    pure module function new_generation_rate(burn_rate, mass_generation_rate, enthalpy_generation_rate)
      implicit none
      real(rkind), intent(in) :: burn_rate, mass_generation_rate, enthalpy_generation_rate
      type(generation_rate_t) new_generation_rate
    end function

    pure module function burn_rate(this)
      implicit none
      class(generation_rate_t), intent(in) :: this
      real(rkind) burn_rate
    end function

    pure module function m_dot_gen(this)
      implicit none
      class(generation_rate_t), intent(in) :: this
      real(rkind) m_dot_gen
    end function

    pure module function e_dot_gen(this)
      implicit none
      class(generation_rate_t), intent(in) :: this
      real(rkind) e_dot_gen
    end function

  end interface

end module generation_rate_interface
