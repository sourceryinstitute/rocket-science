module gas_module
  !! Encapsulate gas thermodynamic state and state relations
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : DP

  implicit none

  private
  public :: gas_t  !! type
  public :: define !! define gas_t components
  public :: c_p    !! specific heat capacity at constant pressure
  public :: MW     !! molecular weight
  public :: T      !! temperature
  public :: h      !! specific enthalpy
  public :: e      !! specific internal energy
  public :: R_gas  !! gas constant
  public :: c_v    !! specific heart at constant volume
  public :: g      !! ratio of specific heat capacities
  public :: p      !! absolute pressure

  type gas_t
    !! encapsulate gas thermodynamic state
    private
    real(DP) c_p, MW, T
  end type

  interface define
    module procedure define_gas
  end interface

contains

  subroutine define_gas(this, input_file)
    !! Read gas components from input file
    type(gas_t), intent(out) :: this
    character(len=*), intent(in) :: input_file
    character(len=max_errmsg_len) error_message
    real(DP) :: c_p, MW, temperature
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    namelist/gas/ c_p, MW, temperature

    open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "gas%define: io_status == success", diagnostic_data=error_message)
    read(file_unit, nml=gas)
    close(file_unit)

    this%c_p = c_p
    this%MW = MW
    this%T = temperature
  end subroutine

  function c_p(this) result(this_c_p)
    !! Result is the specific heat capacity at constant pressure
    type(gas_t), intent(in) :: this
    real(DP) :: this_c_p
    this_c_p = this%c_p
  end function

  function MW(this) result(this_MW)
    !! Result is the gas molecular weight
    type(gas_t), intent(in) :: this
    real(DP) :: this_MW
    this_MW = this%MW
  end function

  function T(this) result(this_temperature)
    !! Result is the gas temperature
    type(gas_t), intent(in) :: this
    real(DP) this_temperature
    this_temperature = this%T
  end function

  function R_gas(this) result(Rgas)
    !! Result is the gas constant
    type(gas_t), intent(in) :: this
    real(DP) Rgas
    real(DP), parameter :: R_universal = 8314._DP ! 8.31446261815324_DP
    Rgas = R_universal/this%MW
  end function

  function c_v(this) result(this_c_v)
    !! Result is the specific heat capacity at constant volume
    type(gas_t), intent(in) :: this
    real(DP) this_c_v
    this_c_v = this%c_p - R_gas(this)
  end function

  function g(this) result(gamma)
    !! Result is the ratio of specific heat capacities
    type(gas_t), intent(in) :: this
    real(DP) gamma
    gamma = this%c_p/c_v(this)
  end function

  function h(this) result(enthalpy)
    !! Result is the specific enthalpy
    type(gas_t), intent(in) :: this
    real(DP) enthalpy
    enthalpy = this%c_p*this%T
  end function

  function e(this) result(internal_energy)
    !! Result is the specific internal energy
    type(gas_t), intent(in) :: this
    real(DP) internal_energy
    internal_energy = c_v(this)*this%T
  end function

  function p(this, mass, volume) result(pressure)
    type(gas_t), intent(in) :: this
    real(DP), intent(in) :: mass, volume
    real(DP) pressure

    associate(rho => mass/volume)
      pressure = rho*R_gas(this)*T(this)
    end associate
  end function

end module gas_module
