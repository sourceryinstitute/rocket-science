module gas_module
  use kind_parameters, only : DP

  implicit none
  real(DP), parameter :: Ru=8314_DP
  private

  public :: gas_t
  public :: define
  public :: get_c_p
  public :: get_T
  public :: get_MW
  public :: get_m

  type gas_t
    private
    real(DP) :: c_p, T, MW, m, c_v, rgas, g
  end type

contains

  subroutine define(this, file_name)
    use assertions_interface, only : max_errmsg_len
    type(gas_t), intent(out) :: this
    character(len=*), intent(in) :: file_name
    character(len=max_errmsg_len) error_message
    real(DP) :: c_p, T, MW, m
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    namelist/gas/ c_p, MW, T, m

    open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
    if (io_status /= success) error stop "gas%define: file open failed with message "//error_message

    read(file_unit, nml=gas)
    close(file_unit)

    this%c_p = c_p
    this%MW = MW
    this%T = T
    this%m = m
    this%rgas=Ru/MW
    this%c_v=c_p-this%rgas
    this%g=c_p/this%c_v

   
    print *, this%c_p, this%rgas, this%c_v, this%g 
  end subroutine define

  function get_c_p(this) result(this_c_p)
    type(gas_t), intent(in) :: this
    real(DP) :: this_c_p
    this_c_p = this%c_p
  end function

  function get_MW(this) result(this_MW)
    type(gas_t), intent(in) :: this
    real(DP) :: this_MW

    this_MW = this%MW

  end function

  function get_T(this) result(this_T)
    type(gas_t), intent(in) :: this
    real(DP) :: this_T

    this_T = this%T

  end function

  function get_m(this) result(this_m)
    type(gas_t), intent(in) :: this
    real(DP) :: this_m

    this_m = this%m

  end function

end module gas_module
