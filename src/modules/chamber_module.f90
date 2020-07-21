module chamber_module
  !! Encapsulate the chamber gas extensive properties (mass & volume, the gas intensive properties,
  !! the combustion model, and the hole geometry.
  use assertions_interface, only : assert, max_errmsg_len
  use gas_module, only : gas_t, define, c_v, R_gas, T, p, g, h, MW, c_p
  use combustion_module, only : combustion_t, define, burn_rate, gen_height, gen_dia, ntabs, rho_solid, m_pkg, T_flame
  use hole_module, only : hole_t, define, area
  use kind_parameters, only : DP
  implicit none

  private
  public :: chamber_t
  public :: define
  public :: mass
  public :: energy
  public :: burn_rate
  public :: generate
  public :: efflux
  public :: p
  public :: T

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

  interface burn_rate
    module procedure chamber_burn_rate
  end interface

  interface p
    module procedure chamber_pressure
  end interface

  interface T
    module procedure chamber_temperature
  end interface

contains

  subroutine define_chamber(this, input_file)
    !! Set all chamber components
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

  function mass(this) result(this_mass)
    !! Result is the gas mass
    type(chamber_t), intent(in) :: this
    real(DP) this_mass
    this_mass = this%M
  end function

  function energy(this) result(this_energy)
    !! Result is the gas internal energy
    type(chamber_t), intent(in) :: this
    real(DP) this_energy
    this_energy = this%M*c_v(this%gas)*T(this%gas)
  end function

  function chamber_pressure(this) result(pressure)
    type(chamber_t), intent(in) :: this
    real(DP) pressure
    pressure = p(this%gas, rho=this%M/this%V)
  end function

  function chamber_temperature(this) result(this_temperature)
    type(chamber_t), intent(in) :: this
    real(DP) this_temperature
    this_temperature = T(this%gas)
  end function

  function chamber_burn_rate(this) result(this_burn_rate)
    !! Result is the rate of surface-normal depth loss for the burning tablets
    type(chamber_t), intent(in) :: this
    real(DP) this_burn_rate
    this_burn_rate = burn_rate(this%combustion, p(this%gas, rho=this%M/this%V))
  end function

  function generate(this, depth, dt) result(rate)
    !! Result contains the burn rate, mass generation rate, and energy generation rate
    use generation_rate_module, only : generation_rate_t
    use universal_constants, only : pi
    type(chamber_t), intent(in) :: this
    type(generation_rate_t) rate
    real(DP), intent(in) :: depth
    real(DP), intent(in) :: dt

    associate( &
        r => 0.5*gen_dia(this%combustion), & ! original radius
        h => gen_height(this%combustion), & ! original height
        br => burn_rate(this) &
    )
      associate(dn => dt*br, num_tablets => ntabs(this%combustion))
        associate(dn_sum => depth + dn) ! cumulative surface-normal burn distance
          associate(surface => merge(0._DP, num_tablets*2*pi*((r-dn_sum)*(h-2*dn_sum) + (r-dn_sum)**2), any(dn_sum > [r, h/2])))
                ! surface area = { 0 if dn_sum exceeds either (tablet radius or tablet half-height
                !                { # tablets * (area of cylinder shrunken by dn in all directions) otherwise
            associate(m_dot => (br*surface*rho_solid(this%combustion)) * (m_pkg(this%combustion)*MW(this%gas)/1000._DP))
                ! (burn rate * area * density) *  gas yield
              associate(h_dot => m_dot*c_p(this%gas)*T_flame(this%combustion))
                 rate = generation_rate_t(burn_rate = br, mass_generation_rate = m_dot , enthalpy_generation_rate = h_dot)
              end associate
            end associate
          end associate
        end associate
      end associate
    end associate
  end function

  function efflux(this) result(rate)
    !! Result contains the flow rates of mass and energy exiting the chamber through the hole
    use universal_constants, only : atmospheric_pressure
    use flow_rate_module, only : flow_rate_t
    type(chamber_t), intent(in) :: this
    type(flow_rate_t) rate
    real(dp) mdtx

    associate( &
      gx => g(this%gas), &
      px => p(this%gas, rho = this%M/this%V), &
      tx => T(this%gas), &
      rx => R_gas(this%gas), &
      ax => area(this%hole) &
    )
      associate( &
        p_ratio => px/atmospheric_pressure, &
        p_crit  => (2._DP/(gx+1._DP))**(gx/(gx-1._DP)) &
       )
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

    rate = flow_rate_t(mass_outflow_rate = mdtx, energy_outflow_rate = mdtx*h(this%gas))
  end function efflux

end module chamber_module
