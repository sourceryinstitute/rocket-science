program main
  use assertions_interface, only : assert
  use chamber_module, only : chamber_t, define
  use gas_module, only : R_gas
  use kind_parameters, only : rkind
  implicit none
  type(chamber_t) chamber
  real(rkind), parameter :: tolerance=1.E-06_rkind
  character(len=*), parameter :: input_file = "volfil.inp"

  real(rkind) volume_expected, mass_expected

  call read_test_data(input_file, volume_expected, mass_expected)

  call define(chamber, input_file)

  print *,"Test passed."

contains

  subroutine read_test_data(file_name, volume, mass)
    use assertions_interface, only : assert, max_errmsg_len
    use kind_parameters, only : rkind
    !! Read volume & mass from namelist in the named file
    character(len=*), intent(in) :: file_name
    integer, parameter :: success = 0
    integer io_status, file_unit
    character(len=max_errmsg_len) error_message
    real(rkind) volume, mass
    namelist/chamber/ volume, mass

    open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "chamber read_test_data: io_status == success", diagnostic_data=error_message)
    read(file_unit, nml=chamber)
    close(file_unit)
  end subroutine

end program
