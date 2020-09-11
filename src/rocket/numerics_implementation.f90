submodule(numerics_interface) numerics_implementation
  implicit none

contains

  module procedure define
     use assertions_interface, only : assert, max_errmsg_len
     real(rkind) dt_, t_max_
     namelist/numerics_list/ dt_, t_max_

     block
       character(len=max_errmsg_len) error_message
       integer io_status, file_unit
       integer, parameter :: success =0

       open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
       call assert(io_status == success, "numerics_t%define: io_status == success", error_message)
       read(file_unit, nml=numerics_list)
       close(file_unit)
     end block

     this%dt_ = dt_
     this%t_max_ = t_max_
  end procedure

  module procedure dt
    dt = this%dt_
  end procedure

  module procedure t_max
     t_max = this%t_max_
  end procedure

end submodule numerics_implementation
