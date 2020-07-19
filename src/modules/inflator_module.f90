module inflator_module
  use gas_module, only : gas_t, define
  use chamber_module, only : chamber_t, define, generate, efflux, burn_rate
  use numerics_module, only : numerics_t, define, dt, t_max
  use kind_parameters, only : DP
  implicit none
  private

  public :: inflator_t
  public :: define
  public :: t_max
  public :: chamber
  public :: dt
  public :: dState_dt

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

  interface dt
    module procedure dt_inflator
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

  function dt_inflator(this) result(this_dt)
    type(inflator_t), intent(in) :: this
    real(DP) this_dt
    this_dt = dt(this%numerics)
  end function

  function dState_dt(this, state) result(this_dState_dt)
    use persistent_state_module, only : persistent_state_t, set_time, set_burn_depth, set_mass, set_energy, burn_depth
    use flow_rate_module,        only : m_dot_out, e_dot_out
    use generation_rate_module,  only : m_dot_gen, e_dot_gen
    type(inflator_t), intent(in) :: this
    type(persistent_state_t), intent(in) :: state
    type(persistent_state_t) this_dState_dt

    call set_time(this_dState_dt, 1._DP)
    call set_burn_depth(this_dState_dt, burn_rate(this%chamber))

    associate( &
      generation_rate => generate(this%chamber, burn_depth(state), dt(this)), &
      outflow => efflux(this%chamber) &
    )
      call set_mass(      this_dState_dt, m_dot_gen(generation_rate) - m_dot_out(outflow))
      call set_energy(    this_dState_dt, e_dot_gen(generation_rate) - e_dot_out(outflow))
    end associate
  end function


end module inflator_module
