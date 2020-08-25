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
    procedure :: define
    procedure :: surface_area
    procedure :: volume
    procedure :: rho_solid
  end type

contains

  subroutine define(this, input_file)
    class(grain_t), intent(out) :: this
    character(len=*), intent(in) :: input_file

    real(rkind) id_, od_, length_, rho_solid_
    namelist/grain_list/ id_, od_, length_, rho_solid_

    block
      integer, parameter :: success = 0
      character(len=max_errmsg_len) error_message
      integer :: io_status, file_unit

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "grain_t%define: io_status == success", diagnostic_data = error_message)
      read(file_unit, nml=grain_list)
      close(file_unit)
    end block

    this%id_ = id_
    this%od_ = od_
    this%length_ = length_
    this%rho_solid_ = rho_solid_
  end subroutine

  pure function rho_solid(this)
    class(grain_t), INTENT(IN) :: this
    real(rkind) rho_solid
    rho_solid = this%rho_solid_
  end function

  pure function surface_area(this, burn_depth)
    use universal_constants, only : pi
    class(grain_t), intent(in) :: this
    real(rkind), intent(in) :: burn_depth
    real(rkind) surface_area
    integer, parameter :: ends = 2
    real(rkind), parameter :: four=4._rkind

    associate(lost_length => ends*burn_depth)
      associate( &
        grain_length => this%length_ - lost_length, &
        id => this%id_+ lost_length, &
        od => (this%od_) &
      )
        surface_area = merge(0._rkind, pi*(id*grain_length + (od**2-(id+2*burn_depth)**2)/four), id>od .or. grain_length<0._rkind)
      end associate
    end associate
  end function

  pure function volume(this, burn_depth)
    use universal_constants, only : pi
    class(grain_t), intent(in) :: this
    real(rkind), intent(in) :: burn_depth
    real(rkind) volume
    integer, parameter :: ends = 2

    associate( &
      lost_length => ends*burn_depth, &
      ir => this%id_/2 + burn_depth, &
      or => this%od_/2, &
      plenum_volume => 1. - pi*(this%id_/2)**2 * this%length_ & ! set initial volume to 1.
    )
      volume = plenum_volume + pi*(ir**2 * (this%length_ - lost_length) + or**2 * lost_length)
    end associate
  end function

end module grain_module
