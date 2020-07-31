module generant_module
  !! This module encapsulates flags.
  use kind_parameters, only : rkind

  implicit none

  private
  public :: gen
  public :: get_tflame, get_mpkg, get_genmass, get_genheight, get_gendiam, get_rhosolid, get_rref, get_n
  public :: set_ntabs

  type gen
     real(rkind):: tflame, mpkg, genmass, genheight, gendiam, rhosolid, ntabs, rref, prefn
  end type



  contains



  function get_tflame(this) result(this_tflame)
      type(gen), intent(in) :: this
    real(rkind) :: this_tflame
    this_tflame = this%tflame_
  end function

   function get_mpkg(this) result(this_mpgk)
      type(gen), intent(in) :: this
    real(rkind) :: this_mpkg
    this_mpkg = this%mpkg_
  end function

  function get_genmass(this) result(this_genmass)
      type(gen), intent(in) :: this
    real(rkind) :: this_genmass
    this_genmass = this%genmass_
  end function

    function get_genheight(this) result(this_genheight)
      type(gen), intent(in) :: this
    real(rkind) :: this_genheight
    this_genheight = this%genheight_
  end function

    function get_gendiam(this) result(this_gendiam)
      type(gen), intent(in) :: this
    real(rkind) :: this_gendiam
    this_gendiam = this%gendiam_
  end function

    function get_rhosolid(this) result(this_rhosolid)
      type(gen), intent(in) :: this
    real(rkind) :: this_rhosolid
    this_rhosolid = this%rhosolid_
  end function

  !  function get_ntabs(this) result(this_ntabs)  ! this is calculated
  !    type(gen), intent(in) :: this
  !  real(rkind) :: this_ntabs
  !  this_ntabs = this%ntabs_
  !end function

    function get_rref(this) result(this_rref)
      type(gen), intent(in) :: this
    real(rkind) :: this_rref
    this_rref = this%rref_
  end function

    function get_n(this) result(this_n)
      type(gen), intent(in) :: this
    real(rkind) :: this_n
    this_n = this%n_
  end function

    function get_summ(this) result(this_summ)
      type(gen), intent(in) :: this
    real(rkind) :: this_summ
    this_summ = this%summ_
  end function



  !  need these set functions  public :: set_ntabs

  subroutine set_ntabs(this) ! only called once to get ntabs and sets the value
    type(gen), intent(inout) :: this
    real(rkind) :: r,h,mass,m1,rho,vol
    real(rkind), parameter :: pi=3.14159265_rkind

    r    = this%gendiam/2._rkind
    h    = this%genheight
    mass = this%genmass
    rho  = this%rhosolid
    vol  = pi*r**2.0_rkind*h    ! tablet volume (cylinder)
    m1   = vol*this_rhosolid ! tablet mass (kg)

    this%ntabs=mass/m1 ! resulting number of tablets for simulation
  end subroutine set_ntabs

end module generant_module
