module combustion_interface
  use kind_parameters, only : rkind
  implicit none

  private

  type, public :: combustion_t
    private
    real(rkind) T_flame_    !! adiabatic flame temperature
    real(rkind) rho_solid_  !! solid density
    real(rkind) r_ref_      !! reference burn rate
    real(rkind) n_          !! burn-rate exponent
  contains
    procedure :: define, T_flame, rho_solid, burn_rate
  end type

  interface

    module subroutine define(this, input_file)
      !! Define this combustion object by reading values from an input file
      implicit none
      class(combustion_t), intent(out) ::  this
      character(len=*), intent(in) :: input_file
    end subroutine

    pure module function burn_rate(this, p)
      !! Result is the rate of surface-normal depth loss
      implicit none
      class(combustion_t), intent(in) :: this
      real(rkind), intent(in) :: p
      real(rkind) burn_rate
    end function

    pure module function rho_solid(this)
      implicit none
      class(combustion_t), intent(in) :: this
      real(rkind) rho_solid
    end function

    pure module function T_flame(this)
      implicit none
      class(combustion_t), intent(in) :: this
      real(rkind) T_flame
    end function

  end interface

end module combustion_interface
