program main
  use assertions_interface, only : assert
  use pyro_module, only : pyro_t, define, gen_height, gen_dia, gen_mass, rho_solid
  use kind_parameters, only : DP
  use math_constants, only : pi
  implicit none
  type(pyro_t) pyro
  real(DP), parameter :: tolerance=1.E-06_DP

  call define(pyro, "volfil.inp")

  associate(voltab_expected => gen_height(pyro)*pi*0.25*gen_dia(pyro)**2)
    associate(mtab_expected => voltab_expected*rho_solid(pyro))
      associate(ntabs_expected => gen_mass(pyro)/mtab_expected)

      end associate
    end associate
  end associate

  print *,"Test passed."
end program
