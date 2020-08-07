module motor_module
  !! Composite physics abstraction encapsulating a chamber and numerical algorithm
  !! parameters
  use assertions_interface, only : assert, max_errmsg_len
  use chamber_module, only : chamber_t
  use numerics_module, only : numerics_t
  use kind_parameters, only : rkind
  implicit none
  private

  public :: motor_t

  type motor_t
    private
    type(numerics_t) numerics_
    type(chamber_t) chamber_
  contains
    procedure :: output
    procedure :: t_max
    procedure :: chamber
    procedure :: dt
    procedure :: d_dt
  end type

  interface motor_t
    module procedure construct_motor_t
  end interface

contains

  function construct_motor_t(input_file) result(new_motor_t)
    !! result ia a newly constructed motor_t object
    character(len=*), intent(in) :: input_file
    type(motor_t) new_motor_t

    new_motor_t%numerics_ = numerics_t(input_file)
    new_motor_t%chamber_ = chamber_t(input_file)
  end function

  subroutine output(this, time, file_unit)
    !! write all motor components to the specified file unit
    class(motor_t), intent(in) :: this
    real(rkind), intent(in) :: time
    integer, intent(in) :: file_unit
    write(file_unit,*) time, this%chamber_%p(), this%chamber_%T()
  end subroutine

  pure function t_max(this) result(this_t_max)
    !! Result is the desired simulation end time
    class(motor_t), intent(in) :: this
    real(rkind) this_t_max
    this_t_max = this%numerics_%t_max()
  end function

  pure function chamber(this) result(this_chamber)
    !! Result is the chamber_t component of this motor
    class(motor_t), intent(in) :: this
    type(chamber_t) this_chamber
    this_chamber = this%chamber_
  end function

  pure function dt(this) result(this_dt)
    !! Result is the simulation time step
    class(motor_t), intent(in) :: this
    real(rkind) this_dt
    this_dt = this%numerics_%dt()
  end function

  pure function d_dt(this, state) result(this_dState_dt)
    !! Result contains the numerically evaluated time derivative of each state variable
    use persistent_state_module, only : persistent_state_t
    use flow_rate_module,        only : flow_rate_t
    use generation_rate_module,  only : generation_rate_t
    class(motor_t), intent(in) :: this
    type(persistent_state_t), intent(in) :: state
    type(persistent_state_t) this_dState_dt

    call this_dState_dt%set_time(1._rkind)
    call this_dState_dt%set_burn_depth(this%chamber_%burn_rate())

    associate( &
      generation_rate => this%chamber_%generate(state%burn_depth(), this%dt()), &
      outflow => this%chamber_%efflux() &
    )
       call this_dState_dt%set_mass(generation_rate%m_dot_gen() - outflow%m_dot_out())
       call this_dState_dt%set_energy(generation_rate%e_dot_gen() - outflow%e_dot_out())
    end associate
  end function

end module motor_module
