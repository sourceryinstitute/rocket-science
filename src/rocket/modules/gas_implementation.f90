submodule(gas_interface) gas_implementation
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

contains

  module procedure define
    real(rkind) :: c_p_, MW_
    namelist/gas_list/ c_p_, MW_

    block
      character(len=max_errmsg_len) error_message
      integer :: io_status, file_unit
      integer, parameter :: success = 0
      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "gas%define: io_status == success", diagnostic_data=error_message)
      read(file_unit, nml=gas_list)
      close(file_unit)
    end block

    this%c_p_ = c_p_
    this%MW_ = MW_
  end procedure

  module procedure T
    T = energy/(this%c_v()*mass)
  end procedure

  module procedure R_gas
    real(rkind), parameter :: R_universal = 8314._rkind ! 8.31446261815324_rkind
    R_gas = R_universal/this%MW_
  end procedure

  module procedure c_v
    c_v = this%c_p_ - this%R_gas()
  end procedure

  module procedure g
    g = this%c_p_/this%c_v()
  end procedure

  module procedure c_p
    c_p = this%c_p_
  end procedure

  module procedure p
    associate(M => (mass), R_gas => this%R_gas(), T => energy/(mass*this%c_v()), V => (volume))
      p = M*R_gas*T/V
    end associate
  end procedure

end submodule gas_implementation
