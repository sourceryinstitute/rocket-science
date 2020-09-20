module nozzle_t_interface
  !! Model the nozzle geometry and flow characteristics
  use constants, only : dp
  implicit none

  private

  type, public :: nozzle_t
    !! Encapsulate the nozzle throad area, discharge coefficient, and thrust calculation
    private
    real(dp) area_ !! throat area
    real(dp) C_f_  !! flow discharge coefficient
  contains
    procedure :: thrust => calcthrust
    procedure :: area
  end type

  interface nozzle_t !! Generic interface for a user-defined structure constructor
    module procedure new_nozzle_t
  end interface

  interface

    pure module function new_nozzle_t(dia, C_f)
      !! Result is a new nozzle_t object defined by the throat diameter and discharge coefficients provided as dummy arguments
      implicit none
      real(dp), intent(in) ::  dia !! throat diameter
      real(dp), intent(in) ::  C_f !! discharge coefficient
      type(nozzle_t) new_nozzle_t
    end function

    pure module function calcthrust(this, p)
      !! Result is the nozzle thrust for the provided dummy-argumet chamber gas pressure
      implicit none
      class(nozzle_t), intent(in) :: this
      real(dp), intent(in) :: p
      real(dp) calcthrust
    end function

    pure module function area(this)
      !! Result is the throat area for this nozzle_t object
      implicit none
      class(nozzle_t), intent(in) :: this
      real(dp) area
    end function

  end interface

end module nozzle_t_interface
