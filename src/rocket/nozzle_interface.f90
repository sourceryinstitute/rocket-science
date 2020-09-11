module nozzle_interface
  !! Encapsulate nozzle geometry and geometrical calculations
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind

  implicit none

  private

  type, public ::nozzle_t
    private
    real(rkind) diameter_
    real(rkind) C_f_        !! thrust coefficient
  contains
    procedure :: define, diameter, area, thrust
  end type

  interface

    module subroutine define(this, input_file)
      implicit none
      class(nozzle_t), intent(out) :: this
      character(len=*), intent(in) :: input_file
    end subroutine

    pure module function diameter(this)
      implicit none
      class(nozzle_t), intent(in) :: this
      real(rkind) diameter
    end function

    elemental module function area(this)
      use universal_constants, only : pi
      implicit none
      class(nozzle_t), intent(in) :: this
      real(rkind) area
    end function

    elemental module function thrust(this, gage_pressure)
      implicit none
      class(nozzle_t), intent(in) :: this
      real(rkind), intent(in) :: gage_pressure
      real(rkind) thrust
    end function

  end interface

end module nozzle_interface
