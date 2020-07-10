module inflator_module
  use gas_module, only : gas_t
  use pyro_module, only : pyro_t
  use chamber_module, only : chamber_t
  use numerics_module, only : numerics_t
  implicit none
  private

  public :: inflator_t

  type inflator_t
    private
    type(gas_t) gas
    type(numerics_t) numerics
    type(chamber_t) chamber
    type(pyro_t) pyro
  end type

end module inflator_module
