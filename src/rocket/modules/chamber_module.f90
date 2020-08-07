module chamber_module
  !! Encapsulate the chamber gas extensive properties (mass & volume, the gas intensive properties,
  !! the combustion model, and the nozzle geometry.
  use assertions_interface, only : assert, max_errmsg_len
  use gas_module, only : gas_t
  use combustion_module, only : combustion_t
  use nozzle_module, only : nozzle_t
  use kind_parameters, only : rkind
  implicit none

  private
  public :: chamber_t

  type grain_t
    !! Encapsulate propellent abstraction in a friend of the chamber_t
    real(rkind) id_, od_, length_, rho_solid_
  end type

  type chamber_t
    private
    real(rkind) M_, V_ ! chamber gas mass and volume (extensive properties)
    type(grain_t) grain_
    type(gas_t) gas_
    type(combustion_t) combustion_
    type(nozzle_t) nozzle_
  contains
    procedure :: mass
    procedure :: energy
    procedure :: efflux
    procedure :: generate
    procedure :: burn_rate
    procedure :: p
    procedure :: T
  end type

  interface chamber_t
    module procedure construct_chamber_t
  end interface

contains

  function construct_chamber_t(input_file) result(new_chamber_t)
    !! Set all chamber components
    character(len=*), intent(in) :: input_file
    type(chamber_t) new_chamber_t
    real(rkind) :: volume, id, od, length, rho_solid
    namelist/chamber/ volume, id, od, length, rho_solid

    block
      integer, parameter :: success = 0
      character(len=max_errmsg_len) error_message
      integer :: io_status, file_unit

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "chamber%define: io_status == success", diagnostic_data = error_message)
      read(file_unit, nml=chamber)
      close(file_unit)
    end block

    new_chamber_t%V_ = volume
    new_chamber_t%grain_ = grain_t(id_=id, od_=od, length_=length, rho_solid_=rho_solid)
    new_chamber_t%gas_ = gas_t(input_file)
    new_chamber_t%combustion_ = combustion_t(input_file)
    new_chamber_t%nozzle_ = nozzle_t(input_file)
  end function

  pure function mass(this) result(this_mass)
    !! Result is the gas mass
    class(chamber_t), intent(in) :: this
    real(rkind) this_mass
    this_mass = this%M_
  end function

  pure function energy(this) result(this_energy)
    !! Result is the gas internal energy
    class(chamber_t), intent(in) :: this
    real(rkind) this_energy
    this_energy = this%M_*this%gas_%c_v()*this%gas_%T()
  end function

  pure function p(this) result(pressure)
    class(chamber_t), intent(in) :: this
    real(rkind) pressure
    pressure = this%gas_%p(rho=this%M_/this%V_)
  end function

  pure function t(this) result(this_temperature)
    class(chamber_t), intent(in) :: this
    real(rkind) this_temperature
    this_temperature = this%gas_%T()
  end function

  pure function burn_rate(this) result(this_burn_rate)
    !! Result is the rate of surface-normal depth loss for the burning tablets
    class(chamber_t), intent(in) :: this
    real(rkind) this_burn_rate
    this_burn_rate = this%combustion_%burn_rate(this%gas_%p(rho=this%M_/this%V_))
  end function

  pure function generate(this, depth, dt) result(rate)
    !! Result contains the burn rate, mass generation rate, and energy generation rate
    use generation_rate_module, only : generation_rate_t
    use universal_constants, only : pi
    class(chamber_t), intent(in) :: this
    type(generation_rate_t) rate
    real(rkind), intent(in) :: depth
    real(rkind), intent(in) :: dt

    associate( &
        r => 0.5*this%combustion_%gen_dia(), & ! original radius
        h => this%combustion_%gen_height(), & ! original height
        br => this%burn_rate() &
    )
      associate(dn => dt*br, num_tablets => this%combustion_%ntabs())
        associate(dn_sum => depth + dn) ! cumulative surface-normal burn distance
          associate(surface => merge(0._rkind, num_tablets*2*pi*((r-dn_sum)*(h-2*dn_sum) + (r-dn_sum)**2), any(dn_sum > [r, h/2])))
                ! surface area = { 0 if dn_sum exceeds either (tablet radius or tablet half-height
                !                { # tablets * (area of cylinder shrunken by dn in all directions) otherwise
            associate(m_dot => (br*surface*this%combustion_%rho_solid()) * (this%combustion_%m_pkg()*this%gas_%MW()/1000._rkind))
                ! (burn rate * area * density) *  gas yield
              associate(h_dot => m_dot*this%gas_%c_p()*this%combustion_%T_flame())
                 rate = generation_rate_t(burn_rate = br, mass_generation_rate = m_dot , enthalpy_generation_rate = h_dot)
              end associate
            end associate
          end associate
        end associate
      end associate
    end associate
  end function

  pure function efflux(this) result(rate)
    !! Result contains the flow rates of mass and energy exiting the chamber through the nozzle
    use universal_constants, only : atmospheric_pressure
    use flow_rate_module, only : flow_rate_t
    class(chamber_t), intent(in) :: this
    type(flow_rate_t) rate
    real(rkind) mdtx

    associate( &
      gx => this%gas_%g(), &
      px => this%gas_%p(rho = this%M_/this%V_), &
      tx => this%gas_%T(), &
      rx => this%gas_%R_gas(), &
      ax => this%nozzle_%area() &
    )
      associate( &
        p_ratio => px/atmospheric_pressure, &
        p_crit  => (2._rkind/(gx+1._rkind))**(gx/(gx-1._rkind)) &
       )
       associate(choked_flow => (1._rkind / p_ratio) < p_crit)
         if (choked_flow) then
           associate(cstar => sqrt((1._rkind / gx) * ((gx + 1._rkind) / 2._rkind) ** ((gx + 1._rkind) / (gx - 1._rkind)) * rx * tx))
             mdtx = px * ax / cstar
           end associate
         else
           associate(facx => p_ratio ** ((gx - 1._rkind) / gx))
             associate(term1 => sqrt(gx * rx * tx / facx), term2 => sqrt((facx - 1._rkind) / (gx - 1._rkind)))
               mdtx = SQRT(2._rkind) * px / p_ratio / rx / tx * facx * term1 * term2 * ax
             end associate
           end associate
         endif
       end associate
     end associate
    end associate

    rate = flow_rate_t(mass_outflow_rate = mdtx, energy_outflow_rate = mdtx*this%gas_%h())
  end function efflux

end module chamber_module
