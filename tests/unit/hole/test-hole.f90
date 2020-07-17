program main
  use assertions_interface, only : assert, max_errmsg_len
  use universal_constants, only : pi
  use hole_module, only :  hole_t, define, diameter, area
  use kind_parameters, only : DP
  implicit none
  type(hole_t) hole
  real(DP), parameter :: tolerance=1.E-6_DP
  character(len=*), parameter :: input_file = "volfil.inp"
  character(len=max_errmsg_len) error_message
  real(DP) :: diameter_expected
  integer :: io_status, file_unit
  integer, parameter :: success = 0

  call read_test_data(input_file, diameter_expected)

  ! Define hole from input file:
  call define(hole, input_file)

  ! Test getter:
  call assert(abs((diameter(hole) - diameter_expected)/diameter_expected) <= tolerance, &
             "abs((diameter(hole) - diameter_expected)/diameter_expected) <= tolerance")

  ! Test area function:
  associate(expected_area => pi*diameter_expected**2/4._DP)
    call assert(abs(area(hole) - expected_area)/expected_area <= tolerance, &
               "abs(area(hole) - expected_area)/expected_area <= tolerance")
  end associate

  print *, "Test passed"

contains

  subroutine read_test_data(file_name, diameter)
    use assertions_interface, only : assert, max_errmsg_len
    use kind_parameters, only : DP
    !! Read diameter from namelist in the named file
    character(len=*), intent(in) :: file_name
    integer, parameter :: success = 0
    integer io_status, file_unit
    character(len=max_errmsg_len) error_message
    real(DP) diameter
    namelist/hole/ diameter

    open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "hole read_test_data: io_status == success", diagnostic_data=error_message)
    read(file_unit, nml=hole)
    close(file_unit)
  end subroutine

end program
