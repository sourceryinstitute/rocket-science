submodule(chamber_interface) chamber_implementation
  !! Encapsulate the chamber components: propellant grain, combustion model, gas, & nozzle geometry
  use assertions_interface, only : assert, max_errmsg_len
  use gas_interface, only : gas_t
  use combustion_interface, only : combustion_t
  use nozzle_module, only : nozzle_t
  use grain_interface, only : grain_t
  use kind_parameters, only : rkind
  implicit none

  real(rkind), parameter :: T_ambient=300._rkind, p_ambient=101325._rkind

contains

  module procedure define
    call this%grain_%define(input_file)
    call this%gas_%define(input_file)
    call this%combustion_%define(input_file)
    call this%nozzle_%define(input_file)
  end procedure

  module procedure gas
    gas = this%gas_
  end procedure

  module procedure volume
    volume = this%grain_%volume(burn_depth)
  end procedure

  module procedure temperature
    temperature = this%gas_%T(energy, mass)
  end procedure

  module procedure thrust
    thrust = this%nozzle_%thrust(gage_pressure=pressure-p_ambient)
  end procedure

  module procedure initial_volume
    real(rkind) initial_volume
    initial_volume = this%grain_%volume(burn_depth=0._rkind)
  end procedure

  module procedure burn_rate
    !! Result is the rate of surface-normal depth loss for the burning tablets
    use state_interface, only : state_t

    associate(burn_depth=>state%burn_depth())
      associate(e => state%energy(), m => state%mass(), V => this%grain_%volume(burn_depth))
        burn_rate = &
          merge(0._rkind, this%combustion_%burn_rate(this%gas_%p(energy=e, mass=m, volume=V)), this%grain_%burned_out(burn_depth))
      end associate
    end associate
  end procedure

  module procedure generate
    !! Result contains the burn rate, mass generation rate, and energy generation rate
    use state_interface, only : state_t
    use generation_rate_interface, only : generation_rate_t

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
  end procedure

  module procedure outflow
    use flow_rate_interface, only : flow_rate_t
    use state_interface, only : state_t

    associate(e => state%energy(), m => state%mass())
      associate(c_p => this%gas_%c_p(), T=>this%gas_%T(e,m), m_dot => this%mdotos(state))
        rate = flow_rate_t(mass_outflow_rate = m_dot, energy_outflow_rate = m_dot*c_p*T)
      end associate
    end associate

  end procedure

  module procedure mdotos
    use state_interface, only : state_t
    real(rkind), parameter :: p2 = p_ambient, sqrt_2 = sqrt(2._rkind)

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
                  mdotos = px*ax/c_star
                end associate
              else
                associate(facx =>p_ratio**((gx-1.)/gx))
                  associate(term1 => sqrt(gx*rx*tx/facx), term2 => sqrt((facx-1.)/(gx-1.)))
                    mdotos = d_sign_g*sqrt_2*px/p_ratio/rx/tx*facx*term1*term2*ax
                  end associate
                end associate
              end if
            end associate
          end associate
        end associate
      end associate
    end associate
  end procedure

  module procedure pressure
    pressure = this%gas_%p(energy, mass, volume)
  end procedure

end submodule chamber_implementation
