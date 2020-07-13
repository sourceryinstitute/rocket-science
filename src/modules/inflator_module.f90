module inflator_module
  use gas_module, only : gas_t, define
  use pyro_module, only : pyro_t, define
  use chamber_module, only : chamber_t, define
  use numerics_module, only : numerics_t, define
  implicit none
  private

  public :: inflator_t
  public :: define

  type inflator_t
    private
    type(gas_t) gas
    type(numerics_t) numerics
    type(chamber_t) chamber
    type(pyro_t) pyro
  end type

  interface define
    module procedure define_inflator
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

end module inflator_module
