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
    real(DP) :: cp, mw, cv, Rgas, h, e, g
  end type gasprop

contains

  function get_cp(this) result(this_cp)
    type(gasprop), intent(in) :: this
    real(DP) :: this_cp
    this_cp = this%cp
  end function

  function get_mw(this) result(this_mw)
    type(gasprop), intent(in) :: this
    real(DP) :: this_mw
    this_mw = this%mw
  end function

  subroutine define(this, input_file, chamber)
    use internal_chamber_module, only : internal_chamber, get_T
    type(gasprop), intent(inout) :: this
    character(len=*), intent(in) :: input_file
    type(internal_chamber), intent(in) :: chamber
    real(DP) :: e, h, Rgas, g, T
    real(DP), parameter :: Ru = 8314._DP !! universal gas constant

    namelist /gasprop/ cp, mw

    open(newunit=u, file=input_file, status='old')
    read(u, nml=gasprop)
    close(u)

    this%cp = cp
    this%mw = mw
    this%cv = this%cp - Ru/this%mw
    this%Rgas = this%cp - this%cv

    T = get_T(chamber)
    this%h = this%cp*T
    this%e = this%cv*T
    this%g = this%cp/this%cv
  end subroutine

end module
