submodule(nozzle_interface) nozzle_implementation
  !! Encapsulate nozzle geometry and geometrical calculations
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

contains

   module procedure define
      real(rkind) dia_ , C_f_
      namelist/nozzle_list/ dia_, C_f_

      block
        character(len=max_errmsg_len) error_message
        integer :: io_status, file_unit
        integer, parameter :: success = 0

        open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
        call assert(io_status == success, "nozzle_t%define: io_status == success ", diagnostic_data = error_message)
        read(file_unit, nml=nozzle_list)
        close(file_unit)
      end block

      this%diameter_ = dia_
      this%C_f_ = C_f_
   end procedure

   module procedure diameter
     diameter = this%diameter_
   end procedure

   module procedure area
     use universal_constants, only : pi
     area = pi*(this%diameter_**2)/4._rkind
   end procedure

   module procedure thrust
     thrust = gage_pressure*this%area()*this%C_f_ ! correction to thrust (actual vs vacuum thrust)
   end procedure

end submodule nozzle_implementation
