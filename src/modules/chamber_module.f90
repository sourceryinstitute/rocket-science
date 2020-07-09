module chamber_module
  use kind_parameters, only : DP
  implicit none

  private
  public :: chamber_t
  public :: define
  public :: get_volume

  type chamber_t
    private
    real(DP) :: volume, mgas, E, M, edot, mdot, T, P, diam
  end type

contains

  subroutine define(this, file_name)
    use assertions_interface, only : max_errmsg_len
    type(chamber_t), intent(out) :: this
    character(len=*), intent(in) :: file_name
    character(len=max_errmsg_len) error_message
    real(DP) :: volume
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    namelist/chamber/ volume

    open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
    if (io_status /= success) error stop "chamber%define:  "
    read(file_unit, nml=chamber)
    this%volume = volume

    close(file_unit)

  end subroutine

  function get_volume(this) result(this_volume)
    type(chamber_t), intent(in) :: this
    real(DP) :: this_volume
    this_volume = this%volume
  end function

end module chamber_module
