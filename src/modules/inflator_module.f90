module inflator_module
  use gas_module, only : gas_t, define
  use chamber_module, only : chamber_t, define, generate, efflux
  use numerics_module, only : numerics_t, define, dt, t_max
  use kind_parameters, only : DP
  implicit none
  private

  public :: inflator_t
  public :: define
  public :: t_max
  public :: chamber
  public :: state_increment

  type inflator_t
    private
    type(numerics_t) numerics
    type(chamber_t) chamber
  end type

  interface define
    module procedure define_inflator
  end interface

  interface t_max
    module procedure t_max_inflator
  end interface

  interface chamber
    module procedure inflator_chamber
  end interface

contains

  subroutine define_inflator(this, input_file)
    type(inflator_t), intent(inout) :: this
    character(len=*), intent(in) :: input_file

    call define(this%numerics, input_file)
    call define(this%chamber, input_file)
  end subroutine

  function t_max_inflator(this) result(this_t_max)
    type(inflator_t), intent(in) :: this
    real(DP) this_t_max
    this_t_max = t_max(this%numerics)
  end function

  function inflator_chamber(this) result(this_chamber)
    type(inflator_t), intent(in) :: this
    type(chamber_t) this_chamber
    this_chamber = this%chamber
  end function

  function state_increment(this, state) result(delta_state)
    use persistent_state_module, only : persistent_state_t, set_time, set_burn_depth, set_mass, set_energy, burn_depth
    use flow_rate_module,        only : m_dot_out, e_dot_out
    use generation_rate_module,  only : m_dot_gen, e_dot_gen, delta_surface_normal
    type(inflator_t), intent(in) :: this
    type(persistent_state_t), intent(in) :: state
    type(persistent_state_t) delta_state

    associate(dt => dt(this%numerics))

      call set_time(delta_state, dt)

      associate(generation_rate => generate(this%chamber, burn_depth(state)), outflow => efflux(this%chamber))

        call set_burn_depth(delta_state, delta_surface_normal(generation_rate))
        call set_mass(      delta_state, dt*(m_dot_gen(generation_rate) - m_dot_out(outflow)))
        call set_energy(    delta_state, dt*(e_dot_gen(generation_rate) - e_dot_out(outflow)))

      end associate
    end associate
  end function

end module inflator_module
