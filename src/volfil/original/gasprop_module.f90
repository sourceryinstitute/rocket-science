module gasprop_module
  !! this module calculates the gas properties as temperature changes in the chamber
  implicit none

  private
  public :: gasprop
  public :: define
  public :: get_cp
  public :: get_mw

  type gasprop
    private
    real(rkind) :: cp, mw, cv, Rgas, h, e, g
  end type gasprop

contains

  function get_cp(this) result(this_cp)
    type(gasprop), intent(in) :: this
    real(rkind) :: this_cp
    this_cp = this%cp
  end function

  function get_mw(this) result(this_mw)
    type(gasprop), intent(in) :: this
    real(rkind) :: this_mw
    this_mw = this%mw
  end function

  subroutine define(this, input_file, T)
    type(gasprop), intent(inout) :: this
    character(len=*), intent(in) :: input_file
    real(rkind), intent(in) :: T
    real(rkind) :: e, h, Rgas, g
    real(rkind), parameter :: Ru = 8314._rkind !! universal gas constant

    namelist /gasprop/ cp, mw

    open(newunit=u, file=input_file, status='old')
    read(u, nml=gasprop)
    close(u)

    this%cp = cp
    this%mw = mw
    this%cv = this%cp - Ru/this%mw
    this%Rgas = this%cp - this%cv

    this%h = this%cp*T
    this%e = this%cv*T
    this%g = this%cp/this%cv
  end subroutine

end module
