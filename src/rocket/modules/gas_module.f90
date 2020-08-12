module gas_module
  !! Encapsulate gas thermodynamic state and state relations
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind

  implicit none

  private
  public :: gas_t

  type gas_t
    !! encapsulate gas thermodynamic properties
    private
    real(rkind) c_p_, MW_
  contains
    procedure :: define       !! read all gas_t components from a file
    procedure :: T            !! temperature
    procedure :: c_p            !! specific enthalpy
    procedure :: R_gas        !! gas constant
    procedure :: g            !! ratio of specific heat capacities
    procedure :: p            !! absolute pressure
    procedure :: c_v !! specific heart at constant volume
  end type

contains

  subroutine define(this, input_file)
    !! Read gas components from input file
    class(gas_t), intent(out) :: this
    character(len=*), intent(in) :: input_file
    character(len=max_errmsg_len) error_message
    real(rkind) :: c_p, MW
    namelist/gas/ c_p, MW

    block
      integer :: io_status, file_unit
      integer, parameter :: success = 0
      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "gas%define: io_status == success", diagnostic_data=error_message)
      read(file_unit, nml=gas)
      close(file_unit)
    end block

    this%c_p_ = c_p
    this%MW_ = MW
  end subroutine

  pure function T(this, energy) result(this_temperature)
    !! Result is the gas temperature
    class(gas_t), intent(in) :: this
    real(rkind), intent(in) :: energy
    real(rkind) this_temperature

    this_temperature = energy/this%c_v()
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

  pure function c_p(this) result(this_c_p)
    !! Result is the specific enthalpy
    class(gas_t), intent(in) :: this
    real(rkind) this_c_p

    this_c_p = this%c_p_
  end function

  pure function p(this, energy, mass, volume) result(pressure)
    class(gas_t), intent(in) :: this
    real(rkind), intent(in) :: energy, mass, volume
    real(rkind) pressure

    associate( &
      M => (mass), &
      R_gas => this%R_gas(), &
      T => energy/this%c_v(), &
      V => (volume) &
    )
      pressure = M*R_gas*T/V
   end associate
  end function

end module gas_module
