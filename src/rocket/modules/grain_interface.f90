module grain_interface
  !! Encapsulate the grain geometry and material properties
  use kind_parameters, only : rkind
  implicit none

  private

  type, public :: grain_t
    !! Encapsulate propellent abstraction as a friend of grain_t
    private
    real(rkind) id_, od_, length_, rho_solid_
  contains
    procedure :: define, surface_area, volume, rho_solid, burned_out
  end type

  interface

    module subroutine define(this, input_file)
      implicit none
      class(grain_t), intent(out) :: this
      character(len=*), intent(in) :: input_file
    end subroutine

    pure module function burned_out(this, burn_depth)
      implicit none
      class(grain_t), intent(in) :: this
      real(rkind), intent(in) :: burn_depth
      logical burned_out
    end function

    pure module function rho_solid(this)
      implicit none
      class(grain_t), intent(in) :: this
      real(rkind) rho_solid
    end function

    pure module function surface_area(this, burn_depth)
      use universal_constants, only : pi
      implicit none
      class(grain_t), intent(in) :: this
      real(rkind), intent(in) :: burn_depth
      real(rkind) surface_area
    end function

    elemental module function volume(this, burn_depth)
      use universal_constants, only : pi
      implicit none
      class(grain_t), intent(in) :: this
      real(rkind), intent(in) :: burn_depth
      real(rkind) volume
    end function

  end interface

end module grain_interface
