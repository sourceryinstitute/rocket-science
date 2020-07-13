module inflator_module
  use gas_module, only : gas_t, define
  use pyro_module, only : pyro_t, define
  use chamber_module, only : chamber_t, define
  use numerics_module, only : numerics_t, define, set_time, get_time, operator(+), get_tmax, get_dt
  use kind_parameters, only : DP
  implicit none
  private

  public :: inflator_t
  public :: define
  public :: set_time
  public :: get_time
  public :: get_tmax
  public :: increment_time

  type inflator_t
    private
    type(gas_t) gas
    type(numerics_t) numerics
    type(chamber_t) chamber
    type(pyro_t) pyro
  end type

  interface get_tmax
    module procedure get_tmax_inflator
  end interface

  interface get_time
    module procedure get_time_inflator
  end interface

  interface define
    module procedure define_inflator
  end interface

  interface set_time
    module procedure set_time_inflator
  end interface

contains

  subroutine define_inflator(this, input_file)
    type(inflator_t), intent(inout) :: this
    character(len=*), intent(in) :: input_file

    call define(this%gas, input_file)
    call define(this%numerics, input_file)
    call define(this%chamber, input_file)
    call define(this%pyro, input_file)
  end subroutine

  subroutine set_time_inflator(this, t)
    type(inflator_t), intent(inout) :: this
    real(DP), intent(in) :: t
    call set_time(this%numerics, t)
  end subroutine

  function get_tmax_inflator(this) result(tmax)
    type(inflator_t), intent(in) :: this
    real(DP) tmax
    tmax = get_tmax(this%numerics)
  end function

  function get_time_inflator(this) result(t)
    type(inflator_t), intent(in) :: this
    real(DP) t
    t = get_time(this%numerics)
  end function

  subroutine increment_time(this)
    type(inflator_t), intent(inout) :: this
    this%numerics = this%numerics + get_dt(this%numerics)
  end subroutine

end module inflator_module
