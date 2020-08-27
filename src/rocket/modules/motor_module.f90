module motor_module
  !! Composite physics abstraction encapsulating a chamber and numerical algorithm
  !! parameters
  use assertions_interface, only : assert, max_errmsg_len
  use chamber_module, only : chamber_t
  use numerics_module, only : numerics_t
  use state_module, only : state_t
  use kind_parameters, only : rkind
  implicit none
  private

  public :: motor_t

  type motor_t
    private
    type(numerics_t) numerics_
    type(chamber_t) chamber_
  contains
    procedure :: define
    procedure :: t_max
    procedure :: chamber
    procedure :: dt
    procedure :: d_dt
    procedure :: derived_variables
  end type

contains

  subroutine define(this, input_file)
    !! define each motor_t component
    class(motor_t), intent(out) :: this
    character(len=*), intent(in) :: input_file

    call this%numerics_%define(input_file)
    call this%chamber_%define(input_file)
  end subroutine

  pure function derived_variables(this, states)
    !! result contains tabulated pressure, mass flux, and thrust versus time
    class(motor_t), intent(in) :: this
    type(state_t), intent(in) :: states(:)
    real(rkind), allocatable :: derived_variables(:,:)

    associate(t=>states%time(), m=>states%mass(), E=>states%energy(), dn=>states%burn_depth())
      associate(V => this%chamber_%volume(dn))
        associate(p => this%chamber_%pressure(energy=E, mass=m, volume=V))
          associate(thrust => this%chamber_%thrust(p))
            derived_variables = reshape([t,p,thrust], [size(t),3])
          end associate
        end associate
      end associate
    end associate

  end function

  pure function t_max(this)
    !! Result is the desired simulation end time
    class(motor_t), intent(in) :: this
    real(rkind) t_max
    t_max = this%numerics_%t_max()
  end function

  pure function chamber(this)
    !! Result is the chamber_t component of this motor
    class(motor_t), intent(in) :: this
    type(chamber_t) chamber
    chamber = this%chamber_
  end function

  pure function dt(this)
    !! Result is the simulation time step
    class(motor_t), intent(in) :: this
    real(rkind) dt
    dt = this%numerics_%dt()
  end function

  pure function d_dt(this, state) result(dState_dt)
    !! Result contains the numerically evaluated time derivative of each state variable
    use state_module, only : state_t
    use state_rate_module, only : state_rate_t
    class(motor_t), intent(in) :: this
    type(state_t), intent(in) :: state
    type(state_rate_t) dState_dt

    associate( &
      generation_rate => this%chamber_%generate(state), &
      flow_rate => this%chamber_%outflow(state) &
    )
      dState_dt = state_rate_t( &
        time_rate = 1._rkind, &
        mass_rate = generation_rate%m_dot_gen() - flow_rate%m_dot_out(), &
        energy_rate = generation_rate%E_dot_gen() - flow_rate%E_dot_out(), &
        burn_depth_rate = this%chamber_%burn_rate(state) &
      )
    end associate
  end function

end module motor_module
