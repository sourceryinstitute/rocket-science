module chamber_module
  !! Encapsulate the chamber components: propellant grain, combustion model, gas, & nozzle geometry
  use assertions_interface, only : assert, max_errmsg_len
  use gas_module, only : gas_t
  use combustion_module, only : combustion_t
  use nozzle_module, only : nozzle_t
  use grain_module, only : grain_t
  use kind_parameters, only : rkind
  implicit none

  private
  public :: chamber_t

  type chamber_t
    private
    type(grain_t) grain_
    type(gas_t) gas_
    type(combustion_t) combustion_
    type(nozzle_t) nozzle_
  contains
    procedure :: define
    procedure :: gas
    procedure :: initial_volume
    procedure :: outflow
    procedure :: generate
    procedure :: burn_rate
  end type

contains

  subroutine define(this, input_file)
    !! Set all chamber components
    class(chamber_t), intent(out) ::  this
    character(len=*), intent(in) :: input_file
    real(rkind) :: id, od, length, rho_solid
    namelist/chamber/ id, od, length, rho_solid

    block
      integer, parameter :: success = 0
      character(len=max_errmsg_len) error_message
      integer :: io_status, file_unit

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "chamber_t%define: io_status == success", diagnostic_data = error_message)
      read(file_unit, nml=chamber)
      close(file_unit)
    end block

    this%grain_ = grain_t(od=od, id=id, length=length, rho_solid=rho_solid)

    call this%gas_%define(input_file)
    call this%combustion_%define(input_file)
    call this%nozzle_%define(input_file)
  end subroutine

  pure function gas(this)
    class(chamber_t), intent(in) :: this
    type(gas_t) gas
    gas = this%gas_
  end function

  pure function initial_volume(this)
    class(chamber_t), intent(in) :: this
    real(rkind) initial_volume
    initial_volume = this%grain_%volume(burn_depth=0._rkind)
  end function

  pure function burn_rate(this, state)
    !! Result is the rate of surface-normal depth loss for the burning tablets
    use persistent_state_module, only : persistent_state_t
    class(chamber_t), intent(in) :: this
    type(persistent_state_t), intent(in) :: state
    real(rkind) burn_rate

    associate(e => state%energy(), m => state%mass(), V => this%grain_%volume(state%burn_depth()))
      burn_rate = this%combustion_%burn_rate(this%gas_%p(energy=e, mass=m, volume=V))
    end associate
  end function

  pure function generate(this, state) result(rate)
    !! Result contains the burn rate, mass generation rate, and energy generation rate
    use persistent_state_module, only : persistent_state_t
    use generation_rate_module, only : generation_rate_t
    class(chamber_t), intent(in) :: this
    type(persistent_state_t), intent(in) :: state
    type(generation_rate_t) rate

    associate( &
      r => this%burn_rate(state), &
      A => this%grain_%surface_area(state%burn_depth()),  &
      rho => this%grain_%rho_solid() &
    )
      associate( &
        m_dot => rho*r*A, &
        T_flame => this%combustion_%T_flame() &
      )
        associate(h => this%gas_%c_p()*T_flame )
          rate = generation_rate_t( &
            burn_rate = r, &
            mass_generation_rate = m_dot, &
            enthalpy_generation_rate = m_dot*h &
          )
        end associate
      end associate
    end associate
  end function

  pure function outflow(this, state) result(rate)
    !! Result contains the flow rates of mass and energy exiting the chamber through the nozzle
    use universal_constants, only : atmospheric_pressure
    use flow_rate_module, only : flow_rate_t
    use persistent_state_module, only : persistent_state_t

    class(chamber_t), intent(in) :: this
    type(persistent_state_t), intent(in) :: state

    type(flow_rate_t) rate
    real(rkind) mdtx
    real(rkind), parameter :: T_ambient=300._rkind, p_ambient=101325._rkind, sqrt_2=sqrt(2._rkind)
    real(rkind), parameter :: p2 = p_ambient

    associate(e => state%energy(), m => state%mass(), V => this%grain_%volume(state%burn_depth()), c_p => this%gas_%c_p())
      associate(T=>this%gas_%T(e,m), p =>this%gas_%p(e,m,V), ax=>this%nozzle_%area(), gx=>this%gas_%g(), rx=>this%gas_%R_gas())
        associate(p1=>p, p_crit=>(2./(gx+1.))**(gx/(gx-1.)))
          associate( &
            px => max(p1,p2), &
            tx => merge(T, T_ambient,      p1>p2), &
            p_ratio => merge(p1/p2, p2/p1, p1>p2), &
            d_sign_g=> merge(1., -1.,      p1>p2)  &
          )
            associate(hx=>c_p*tx, choked_flow => (1./p_ratio) < p_crit)
              if (choked_flow) then
                associate(c_star => sqrt((1./gx)*((gx+1.)/2.)**((gx+1.)/(gx-1.))*rx*tx))
                  mdtx = px*ax/c_star
                end associate
              else
                associate(facx =>p_ratio**((gx-1.)/gx))
                  associate(term1 => sqrt(gx*rx*tx/facx), term2 => sqrt((facx-1.)/(gx-1.)))
                    mdtx = d_sign_g*sqrt_2*px/p_ratio/rx/tx*facx*term1*term2*ax
                  end associate
                end associate
              end if
              rate = flow_rate_t(mass_outflow_rate = mdtx, energy_outflow_rate = mdtx*c_p*T)
            end associate
          end associate
        end associate
      end associate
    end associate
  end function outflow

end module chamber_module
