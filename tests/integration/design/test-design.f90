program main
  !! Verify the complete object-oriented design: this test
  !! passes if each module has been defined and has the
  !! named type defined inside the module.
  use numerics_module, only : numerics_t
  use gas_t_module, only : gas_t
  use pyro_t_module, only : pyro_t
  use hole_t_module, only : hole_t
  use chamber_t_module, only : chamber_t
  use inflator_t_module, only : inflator_t
  implicit none

  type(numerics_t) numerics
  type(gas_t) gas
  type(pyro_t) pyro
  type(hole_t) hole
  type(chamber_t) chamber
  type(inflator_t) inflator

  print *,"Test passed."
end program main
