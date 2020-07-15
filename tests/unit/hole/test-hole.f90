program main
  use assertions_interface, only : assert, max_errmsg_len
  use hole_module, only :  hole_t, define, get_diameter, area
  use kind_parameters, only : DP
  implicit none
  type(hole_t) hole
  real(DP), parameter :: tolerance=1.E-6_DP, pi=3.141592654_DP

  character(len=max_errmsg_len) error_message
  real(DP) :: diameter
  integer :: io_status, file_unit
  integer, parameter :: success = 0
  namelist/hole_list/ diameter

  ! Read values from file:
  open(newunit=file_unit, file="volfil.inp", status="old", iostat=io_status, iomsg=error_message)
  call assert(io_status == success, "hole%define: io_status == success ", diagnostic_data = error_message)
  read(file_unit, nml=hole_list)
  close(file_unit)

  ! Define hole from input file:
  call define(hole, "volfil.inp")

  ! Test getter:
  call assert(abs((get_diameter(hole) - diameter)/diameter) <= tolerance, &
             "abs((get_diameter(hole) - diameter)/diameter) <= tolerance")

  ! Test area function:
  associate(expected_area => pi*diameter**2/4._DP)
    call assert(abs(area(hole) - expected_area)/expected_area <= tolerance, &
               "abs(area(hole) - expected_area)/expected_area <= tolerance")
  end associate

  print *, "Test passed"
end program
