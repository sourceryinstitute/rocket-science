submodule(motor_interface) motor_implementation
  !! Composite physics abstraction encapsulating a chamber and numerical algorithm
  !! parameters
  use assertions_interface, only : assert, max_errmsg_len
  implicit none

contains

  module procedure define
    call this%numerics_%define(input_file)
    call this%chamber_%define(input_file)
  end procedure

  module procedure derived_variables

    associate(t=>states%time(), m=>states%mass(), E=>states%energy(), dn=>states%burn_depth())
      associate(V => this%chamber_%volume(dn))
        associate( &
          p => this%chamber_%pressure(energy=E, mass=m, volume=V), &
          temperature => this%chamber_%temperature(energy=E, mass=m), &
          mdotos => this%chamber_%mdotos(states) &
          )
          associate(thrust => this%chamber_%thrust(p))
            derived_variables = reshape([t,p,temperature,mdotos,thrust,V], [size(t),6])
          end associate
        end associate
      end associate
    end associate

  end procedure

  module procedure t_max
    t_max = this%numerics_%t_max()
  end procedure

  module procedure chamber
    chamber = this%chamber_
  end procedure

  module procedure dt
    dt = this%numerics_%dt()
  end procedure

  module procedure d_dt
    use kind_parameters, only : rkind
    use state_rate_interface, only : state_rate_t

    associate( &
      generation_rate => this%chamber_%generate(state), &
      outflow_rate => this%chamber_%outflow(state) &
    )
      dState_dt = state_rate_t( &
        time_rate = 1._rkind, &
        mass_rate = generation_rate%m_dot_gen() - outflow_rate%m_dot(), &
        energy_rate = generation_rate%E_dot_gen() - outflow_rate%E_dot(), &
        burn_depth_rate = this%chamber_%burn_rate(state) &
      )
    end associate
  end procedure

end submodule motor_implementation
