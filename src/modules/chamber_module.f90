module chamber_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : DP
  use gas_module, only : gas_t, define, c_v, R_gas, T
  implicit none

  private
  public :: chamber_t
  public :: define
  public :: get_volume
  public :: get_mass
  public :: get_temperature
  public :: get_pressure
  public :: get_internal_energy
  public :: get_gas

  type chamber_t
    private
    real(DP)  M, V ! m_gas, e_dot, m_dot, dia
    type(gas_t) gas
  end type

  interface define
    module procedure define_chamber
  end interface

contains

  subroutine define_chamber(this, input_file)
    type(chamber_t), intent(out) :: this
    character(len=*), intent(in) :: input_file
    real(DP) :: volume, mass
    namelist/chamber/ volume, mass

    block
      integer, parameter :: success = 0
      character(len=max_errmsg_len) error_message
      integer :: io_status, file_unit

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "chamber%define: io_status == success", diagnostic_data = error_message)
      read(file_unit, nml=chamber)
      close(file_unit)
    end block

    this%V = volume
    this%M = mass

    call define(this%gas, input_file)
  end subroutine

  function get_volume(this) result(this_volume)
    type(chamber_t), intent(in) :: this
    real(DP) :: this_volume
    this_volume = this%V
  end function

  function get_mass(this) result(this_mass)
    type(chamber_t), intent(in) :: this
    real(DP) :: this_mass
    this_mass = this%M
  end function

  function get_temperature(this) result(this_temperature)
    type(chamber_t), intent(in) :: this
    real(DP) :: this_temperature
    this_temperature= T(this%gas)
  end function

  function get_pressure(this) result(this_pressure)
    type(chamber_t), intent(in) :: this
    real(DP) :: this_pressure
    associate(rho => this%M/this%V, R => R_gas(this%gas))
      this_pressure = rho*R*T(this%gas)
    end associate
  end function

  function get_internal_energy(this) result(this_energy)
    type(chamber_t), intent(in) :: this
    real(DP) this_energy
    this_energy = this%M*c_v(this%gas)*T(this%gas)
  end function

  function get_gas(this) result(this_gas)
    type(chamber_t), intent(in) :: this
    type(gas_t) this_gas
    this_gas = this%gas
  end function

end module chamber_module
