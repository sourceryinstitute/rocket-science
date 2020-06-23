module gas_module
  use kind_parameters, only : DP

  implicit none
  private

  public :: gas_t
  public :: defineg
  public :: get_gas

  type gas_t
    private
    real(DP) :: c_p, T, MW, m
  end type

contains

  subroutine defineg(this, file_name)
    use assertions_interface, only : max_errmsg_len
    type(gas_t), intent(out) :: this
    character(len=*), intent(in) :: file_name
    character(len=max_errmsg_len) error_message
    real(DP) :: c_p, T, MW, m
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    namelist/gas/ c_p, MW, T, m

    open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
    if (io_status /= success) error stop "gas%define:  "
    read(file_unit, nml=gas)
    this%c_p = c_p
 !   this%MW = MW
 !   this%T = T
 !   this%m = m

    close(file_unit)

  end subroutine defineg

  function get_gas(this) result(this_c_p) !,this_MW,this_T,this_m)
    type(gas_t), intent(in) :: this
    real(DP) :: this_c_p !,this_MW,this_T,this_m
    this_c_p = this%c_p
!    this_MW = this%MW
!    this_T = this%T
!    this_m = this%m
  end function


end module gas_module
