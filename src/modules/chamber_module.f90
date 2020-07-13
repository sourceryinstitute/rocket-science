module chamber_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : DP
  implicit none

  private
  public :: chamber_t
  public :: define
  public :: get_volume
  public :: get_pressure

  type chamber_t
    private
    real(DP) :: V, m_gas, E, M, e_dot, m_dot, T, P, dia
  end type

  interface define
    module procedure define_chamber
  end interface

contains

  subroutine define_chamber(this, input_file)
    type(chamber_t), intent(out) :: this
    character(len=*), intent(in) :: input_file
    character(len=max_errmsg_len) error_message
    real(DP) :: volume, pressure
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    namelist/chamber/ volume, pressure

    open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "chamber%define: io_status == success", diagnostic_data = error_message)
    read(file_unit, nml=chamber)
    close(file_unit)
    this%V = volume
    this%P = pressure
  end subroutine

  function get_volume(this) result(this_volume)
    type(chamber_t), intent(in) :: this
    real(DP) :: this_volume
    this_volume = this%V
  end function

  function get_pressure(this) result(this_pressure)
    type(chamber_t), intent(in) :: this
    real(DP) :: this_pressure
    this_pressure = this%P
  end function

end module chamber_module
