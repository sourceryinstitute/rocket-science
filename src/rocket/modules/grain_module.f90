module grain_module
  !! Encapsulate the grain geometry
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

  private
  public :: grain_t

  type grain_t
    !! Encapsulate propellent abstraction as a friend of grain_t
    private
    real(rkind) id_, od_, length_, rho_solid_
  contains
    procedure :: surface_area
    procedure :: volume
  end type

  interface grain_t
    module procedure construct_grain_t
  end interface

contains

  pure function construct_grain_t(id, od, length, rho_solid) result(new_grain_t)
    real(rkind), intent(in) :: id, od, length, rho_solid
    type(grain_t) new_grain_t

    new_grain_t%id_ = id
    new_grain_t%od_ = od
    new_grain_t%length_ = length
    new_grain_t%rho_solid_ = rho_solid
  end function

  pure function surface_area(this, burn_depth) result(grain_area)
    use universal_constants, only : pi
    class(grain_t), intent(in) :: this
    real(rkind), intent(in) :: burn_depth
    real(rkind) grain_area
    integer, parameter :: ends = 2
    real(rkind), parameter :: four=4._rkind

    associate(lost_length => ends*burn_depth)
      associate( &
        grain_length => this%length_ - lost_length, &
        id => this%id_+ lost_length, &
        od => (this%od_) &
      )
        grain_area = merge(0._rkind, pi*(id*grain_length + (od**2-(id+2*burn_depth)**2)/four), id>od .or. grain_length<0._rkind)
      end associate
    end associate
  end function

  pure function volume(this, burn_depth) result(this_volume)
    use universal_constants, only : pi
    class(grain_t), intent(in) :: this
    real(rkind), intent(in) :: burn_depth
    real(rkind) this_volume
    integer, parameter :: ends = 2

    associate( &
      lost_length => ends*burn_depth, &
      ir => this%id_/2 + burn_depth, &
      or => this%od_/2 &
    )
      this_volume = pi*(ir**2 * (this%length_ - lost_length) + or**2 * lost_length)
    end associate
  end function

end module grain_module
