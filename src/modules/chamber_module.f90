module chamber_module
  use assertions_interface, only : assert, max_errmsg_len
  use gas_module, only : gas_t, define, c_v, R_gas, T, p, g, h
  use combustion_module, only : combustion_t, define
  use hole_module, only : hole_t, define, area
  use kind_parameters, only : DP
  implicit none

  private
  public :: chamber_t
  public :: define
  public :: mass
  public :: energy
  public :: get_volume
  public :: get_temperature
  public :: get_pressure
  public :: get_gas
  public :: m_dot_gen
  public :: e_dot_gen
  public :: efflux

  type chamber_t
    private
    real(DP) M, V
    type(gas_t) gas
    type(combustion_t) combustion
    type(hole_t) hole
  end type

  interface define
    module procedure define_chamber
  end interface

contains

  function m_dot_gen(this, dt) result(this_m_dot_gen)
    type(chamber_t), intent(in) :: this
    real(DP), intent(in) :: dt
    real(DP) this_m_dot_gen
    this_m_dot_gen = 0._DP*dt
  end function

  function e_dot_gen(this, dt) result(this_e_dot_gen)
    type(chamber_t), intent(in) :: this
    real(DP), intent(in) :: dt
    real(DP) this_e_dot_gen
    this_e_dot_gen = 0._DP*dt
  end function

  function efflux(this) result(flow_rate)
    use universal_constants, only : atmospheric_pressure
    use flow_rate_module, only : flow_rate_t, define
    type(chamber_t), intent(in) :: this
    type(flow_rate_t) flow_rate
    real(dp) mdtx

    associate( &
      gx => g(this%gas), &
      px => p(this%gas, mass = this%M, volume = this%V), &
      tx => T(this%gas), &
      rx => R_gas(this%gas), &
      ax => area(this%hole) &
    )
      associate( &
        p_ratio => px/atmospheric_pressure, &
        p_crit  => (2._DP/(gx+1._DP))**(gx/(gx-1._DP)) &
       )
       call assert(p_ratio <= 1._DP, "p_ratio <= 1._DP") ! assert positive flow

       associate(choked_flow => (1._DP / p_ratio) < p_crit)
         if (choked_flow) then
           associate(cstar => sqrt((1._DP / gx) * ((gx + 1._DP) / 2._DP) ** ((gx + 1._DP) / (gx - 1._DP)) * rx * tx))
             mdtx = px * ax / cstar
           end associate
         else
           associate(facx => p_ratio ** ((gx - 1._DP) / gx))
             associate(term1 => sqrt(gx * rx * tx / facx), term2 => sqrt((facx - 1._DP) / (gx - 1._DP)))
               mdtx = SQRT(2._DP) * px / p_ratio / rx / tx * facx * term1 * term2 * ax
             end associate
           end associate
         endif
       end associate
     end associate
    end associate

    call define(flow_rate, mass_outflow_rate = mdtx, energy_outflow_rate = mdtx*h(this%gas))
  end function

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
    call define(this%combustion, input_file)
    call define(this%hole, input_file)
  end subroutine

  function get_volume(this) result(this_volume)
    type(chamber_t), intent(in) :: this
    real(DP) :: this_volume
    this_volume = this%V
  end function

  function mass(this) result(this_mass)
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
    real(DP) this_pressure
    this_pressure = p(this%gas, this%M, this%V)
  end function

  function energy(this) result(this_energy)
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
