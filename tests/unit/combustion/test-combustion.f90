program main
  use assertions_interface, only : assert
  use combustion_module, only : combustion_t, define, gen_height, gen_dia, gen_mass, rho_solid, ntabs
  use kind_parameters, only : rkind
  use universal_constants, only : pi
  implicit none
  type(combustion_t) combustion
  real(rkind), parameter :: tolerance=1.E-06_rkind

  call define(combustion, "volfil.inp")

  test_ntabs: &
  associate(voltab_expected => gen_height(combustion)*pi*0.25_rkind*gen_dia(combustion)**2)
    associate(mtab_expected => voltab_expected*rho_solid(combustion))
      associate(ntabs_expected => gen_mass(combustion)/mtab_expected)

        call assert((ntabs(combustion) - ntabs_expected)/ntabs_expected < tolerance, &
                   "(ntabs(combustion) - ntabs_expected)/ntabs_expected < tolerance")

      end associate
    end associate
  end associate test_ntabs

  print *,"Test passed."
end program
