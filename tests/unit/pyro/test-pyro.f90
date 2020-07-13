program main
  use assertions_interface, only : assert
  use pyro_module, only : pyro_t, define
  use kind_parameters, only : DP
  implicit none
  type(pyro_t) pyro
  real(DP), parameter :: tolerance=1.E-06_DP

  call define(pyro, "volfil.inp")

  print *,"Test passed."
end program
