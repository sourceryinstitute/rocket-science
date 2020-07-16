program main
  use assertions_interface, only : assert
  use chamber_module, only : chamber_t, define, get_volume, mass, get_pressure, get_temperature, get_gas
  use gas_module, only : R_gas
  use kind_parameters, only : DP
  implicit none
  type(chamber_t) chamber
  real(DP), parameter :: tolerance=1.E-06_DP
  character(len=*), parameter :: input_file = "volfil.inp"

  real(DP) volume_expected, mass_expected

  call read_test_data(input_file, volume_expected, mass_expected)

  call define(chamber, input_file)

  call assert(abs((get_volume(chamber) - volume_expected)/volume_expected) <= tolerance, &
             "abs((get_volume(chamber) - volume_expected)/volume_expected) <= tolerance")
  call assert(abs((mass(chamber) - mass_expected)/mass_expected) <= tolerance, &
             "abs((mass(chamber) - mass_expected)/mass_expected) <= tolerance")

  associate(rho => mass(chamber)/get_volume(chamber), T => get_temperature(chamber))
    associate( chamber_gas => get_gas(chamber))
      associate(pressure_expected => rho*R_gas(chamber_gas)*T )
        call assert(abs((get_pressure(chamber) - pressure_expected)/pressure_expected) <= tolerance, &
                   "abs((get_pressure(chamber) - pressure_expected)/pressure_expected) <= tolerance")
      end associate
     end associate
  end associate

  print *,"Test passed."

contains

  subroutine read_test_data(file_name, volume, mass)
    use assertions_interface, only : assert, max_errmsg_len
    use kind_parameters, only : DP
    !! Read volume & mass from namelist in the named file
    character(len=*), intent(in) :: file_name
    integer, parameter :: success = 0
    integer io_status, file_unit
    character(len=max_errmsg_len) error_message
    real(DP) volume, mass
    namelist/chamber/ volume, mass

    open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "chamber read_test_data: io_status == success", diagnostic_data=error_message)
    read(file_unit, nml=chamber)
    close(file_unit)
  end subroutine

end program
