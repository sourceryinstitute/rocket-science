module inflator_module
  use gas_module, only : gas_t, define
  use chamber_module, only : chamber_t, define, m_dot_gen
  use numerics_module, only : numerics_t, define, dt, t_max
  use kind_parameters, only : DP
  implicit none
  private

  public :: inflator_t
  public :: define
  public :: t_max
  public :: step

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

  function step(this, state) result(next_state)
    use persistent_state_module, only : persistent_state_t, set_burn_depth, burn_depth, set_time, time
    type(inflator_t), intent(in) :: this
    type(persistent_state_t), intent(in) :: state
    type(persistent_state_t) next_state

    !associate(m_dot => m_dot_gen(this%chamber, dt))
    !end associate
    call set_burn_depth(next_state, burn_depth(state))
    call set_time(next_state, time(state) + dt(this%numerics))
  end function

end module inflator_module
