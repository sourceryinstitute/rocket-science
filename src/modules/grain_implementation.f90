submodule(grain_interface) grain_implementation
  !! Encapsulate the grain geometry
  use assertions_interface, only : assert, max_errmsg_len
  implicit none

contains

  module procedure define

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
  end procedure

  module procedure burned_out
    integer, parameter :: ends = 2

    associate(lost_length => ends*burn_depth)
      associate( &
        grain_length => this%length_ - lost_length, &
        id => this%id_+ lost_length, &
        od => (this%od_) &
      )
        burned_out = id>od .or. grain_length<0
      end associate
    end associate
  end procedure

  module procedure rho_solid
    rho_solid = this%rho_solid_
  end procedure

  module procedure surface_area
    use universal_constants, only : pi
    integer, parameter :: ends = 2
    real(rkind), parameter :: four=4, zero=0

    associate(lost_length => ends*burn_depth)
      associate( &
        grain_length => this%length_ - lost_length, &
        id => this%id_+ lost_length, &
        od => (this%od_) &
      )
        surface_area = merge(zero, pi*(id*grain_length + ends*(od**2-id**2)/four), this%burned_out(burn_depth))
      end associate
    end associate
  end procedure

  module procedure volume
    use universal_constants, only : pi
    integer, parameter :: ends = 2

    associate( &
      length_new => this%length_ - ends*burn_depth, &
      ir_new => this%id_/2 + burn_depth, &
      ir_original => this%id_/2, &
      or_original => this%od_/2 &
      )
        volume = 1. + length_new*pi*(ir_new**2  - ir_original**2) + ends*burn_depth*pi*(or_original**2 - ir_original**2)
    end associate
  end procedure

end submodule grain_implementation
