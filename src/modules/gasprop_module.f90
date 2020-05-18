module gasprop_module
  !! this module calculates the gas properties as temperature changes in the chamber
  implicit none

  private
  public :: cp,cv,h,e,mw,rgas,g

      type gasprop
          real(DP) :: cp,cv,h,e,mw,rgas,g
      end type gasprop

  contains


function get_cp(this) result(this_cp)
      type(gasprop), intent(in) :: this
      real(DP) :: this_cp
      this_cp = this%cp_
    end function

function get_mw(this) result(this_mw)
      type(gasprop), intent(in) :: this
      real(DP) :: this_mw
      this_mw = this%mw_
    end function

function setgp(this) result(this_cv, this_e, this_h, this_rgas, this_g)
  use chamber_module, only : T, get_T
  type(gasprop), intent(inout) :: this
      real(DP) this_cp, this_cv, this_e, this_h, this,_rgas, this_g


block

    subroutine setgasprop(T,cham)  ! all f(T)
use chamber_module, only : T, get_T
    type(gasprop),intent(inout)::xp
    type(chamber_internal),intent(in)::cham
    real(DP)::T
    T=cham%T
    xp%h=xp%cp*T
    xp%e=xp%cv*T
    xp%rgas=xp%cp-xp%cv
    xp%g=xp%cp/xp%cv
    end subroutine getgasproperties

end block
