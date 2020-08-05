module gas_module
  !! Encapsulate gas thermodynamic state and state relations
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind

  implicit none

  private
  public :: gas_t

  type gas_t
    !! encapsulate gas thermodynamic state
    private
    real(rkind) c_p_, MW_, T_
  contains
    procedure :: c_p    !! specific heat capacity at constant pressure
    procedure :: MW     !! molecular weight
    procedure :: T      !! temperature
    procedure :: h      !! specific enthalpy
    procedure :: e      !! specific internal energy
    procedure :: R_gas  !! gas constant
    procedure :: c_v    !! specific heart at constant volume
    procedure :: g      !! ratio of specific heat capacities
    procedure :: p      !! absolute pressure
  end type

  interface gas_t
    module procedure construct_gas_t
  end interface

contains

  function construct_gas_t(input_file) result(new_gas_t)
    !! Read gas components from input file
    character(len=*), intent(in) :: input_file
    type(gas_t) new_gas_t
    character(len=max_errmsg_len) error_message
    real(rkind) :: c_p, MW, temperature
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    namelist/gas/ c_p, MW, temperature

    open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "gas%construct_gas_t: io_status == success", diagnostic_data=error_message)
    read(file_unit, nml=gas)
    close(file_unit)

    new_gas_t%c_p_ = c_p
    new_gas_t%MW_ = MW
    new_gas_t%T_ = temperature
  end function

  pure function c_p(this) result(this_c_p)
    !! Result is the specific heat capacity at constant pressure
    class(gas_t), intent(in) :: this
    real(rkind) :: this_c_p
    this_c_p = this%c_p_
  end function

  pure function MW(this) result(this_MW)
    !! Result is the gas molecular weight
    class(gas_t), intent(in) :: this
    real(rkind) :: this_MW
    this_MW = this%MW_
  end function

  pure function T(this) result(this_temperature)
    !! Result is the gas temperature
    class(gas_t), intent(in) :: this
    real(rkind) this_temperature
    this_temperature = this%T_
  end function

  pure function R_gas(this) result(this_R_gas)
    !! Result is the gas constant
    class(gas_t), intent(in) :: this
    real(rkind) this_R_gas
    real(rkind), parameter :: R_universal = 8314._rkind ! 8.31446261815324_rkind
    this_R_gas = R_universal/this%MW_
  end function

  pure function c_v(this) result(this_c_v)
    !! Result is the specific heat capacity at constant volume
    class(gas_t), intent(in) :: this
    real(rkind) this_c_v
    this_c_v = this%c_p_ - this%R_gas()
  end function

  pure function g(this) result(this_g)
    !! Result is the ratio of specific heat capacities
    class(gas_t), intent(in) :: this
    real(rkind) this_g
    this_g= this%c_p_/this%c_v()
  end function

  pure function h(this) result(this_enthalpy)
    !! Result is the specific enthalpy
    class(gas_t), intent(in) :: this
    real(rkind) this_enthalpy
    this_enthalpy = this%c_p_*this%T_
  end function

  pure function e(this) result(this_internal_energy)
    !! Result is the specific internal energy
    class(gas_t), intent(in) :: this
    real(rkind) this_internal_energy
    real(rkind) internal_energy
    this_internal_energy = this%c_v()*this%T_
  end function

  pure function p(this, rho) result(this_pressure)
    class(gas_t), intent(in) :: this
    real(rkind), intent(in) :: rho ! density
    real(rkind) this_pressure
    this_pressure = rho*this%R_gas()*this%T_
  end function

end module gas_module
