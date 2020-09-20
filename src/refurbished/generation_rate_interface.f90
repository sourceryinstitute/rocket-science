module generation_rate_interface
  !! Describe the gas-liberation from the burning propellant
  use constants, only : dp
  implicit none

  private

  type, public :: generation_rate_t
    !! Encapsulate the propellant mass and energy liberation
    private
    real(dp) mdotgen_ ! mass liberation
    real(dp) edotgen_ ! energy liberation
  contains
    procedure :: mdotgen
    procedure :: edotgen
  end type

  interface generation_rate_t !! Generic interface for user-defined structure constructors
    module procedure calmdotgen
  end interface

  interface

    pure module function calmdotgen(rhos, r, surf, cp, Tflame) result(new_generation_rate)
      !! Result is a new generation_rate_t objected
      implicit none
      real(dp), intent(in) :: rhos, r, surf, cp, Tflame
      type(generation_rate_t) new_generation_rate
    end function

    pure module function mdotgen(this)
      !! Result is the generated mass value from this generation_rate_t object
      implicit none
      class(generation_rate_t), intent(in) :: this
      real(dp) mdotgen
    end function

    pure module function edotgen(this)
      !! Result is the generated energy value from this generation_rate_t object
      implicit none
      class(generation_rate_t), intent(in) :: this
      real(dp) edotgen
    end function

  end interface

end module generation_rate_interface
