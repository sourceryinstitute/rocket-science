module gas_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : DP

  implicit none
  private

  public :: gas_t
  public :: define
  public :: c_p
  public :: MW
  public :: R_gas
  public :: c_v
  public :: g
  public :: h
  public :: e

  type gas_t
    private
    real(DP) c_p, MW
  end type

  interface define
    module procedure define_gas
  end interface

contains

  subroutine define_gas(this, input_file)
    type(gas_t), intent(out) :: this
    character(len=*), intent(in) :: input_file
    character(len=max_errmsg_len) error_message
    real(DP) :: c_p, MW
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    namelist/gas/ c_p, MW

    open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "gas%define: io_status == success", diagnostic_data=error_message)
    read(file_unit, nml=gas)
    close(file_unit)

    this%c_p = c_p
    this%MW = MW
  end subroutine

  function c_p(this) result(this_c_p)
    type(gas_t), intent(in) :: this
    real(DP) :: this_c_p
    this_c_p = this%c_p
  end function

  function MW(this) result(this_MW)
    type(gas_t), intent(in) :: this
    real(DP) :: this_MW
    this_MW = this%MW
  end function

  function R_gas(this) result(Rgas)
    type(gas_t), intent(in) :: this
    real(DP) Rgas
    real(DP), parameter :: R_universal = 8314._DP ! 8.31446261815324_DP
    Rgas = R_universal/this%MW
  end function

  function c_v(this) result(this_c_v)
    type(gas_t), intent(in) :: this
    real(DP) this_c_v
    this_c_v = this%c_p - R_gas(this)
  end function

  function g(this) result(gamma)
    type(gas_t), intent(in) :: this
    real(DP) gamma
    gamma = this%c_p/c_v(this)
  end function

  function h(this,T) result(enthalpy)
    type(gas_t), intent(in) :: this
    real(DP), intent(in) :: T
    real(DP) enthalpy
    enthalpy = this%c_p*T
  end function

  function e(this,T) result(internal_energy)
    type(gas_t), intent(in) :: this
    real(DP), intent(in) :: T
    real(DP) internal_energy
    internal_energy = c_v(this)*T
  end function

end module gas_module
