submodule(combustion_interface) combustion_implementation
  use assertions_interface, only : assert, max_errmsg_len
  implicit none

contains

  module procedure define
    real(rkind) T_flame_, r_ref_, n_
    namelist/combustion_list/ T_flame_, r_ref_, n_

    block
      integer :: io_status, file_unit
      integer, parameter :: success = 0
      character(len=max_errmsg_len) error_message

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "combustion%define: io_status == success ", diagnostic_data = error_message)
      read(file_unit, nml=combustion_list)
      close(file_unit)
    end block

    this%T_flame_ = T_flame_
    this%r_ref_ = r_ref_
    this%n_ = n_
  end procedure

  module procedure burn_rate
    real(rkind), parameter :: p_ref = 3000._rkind*6894.76  !! reference pressure 3000 psia converted to pascals
    burn_rate = this%r_ref_*(p/p_ref)**this%n_ ! (ref. rate) * (chamber pressure / ref. pressure)**(rate_exponent)
  end procedure

  module procedure rho_solid
    rho_solid = this%rho_solid_
  end procedure

  module procedure T_flame
    T_flame = this%T_flame_
  end procedure

end submodule combustion_implementation
