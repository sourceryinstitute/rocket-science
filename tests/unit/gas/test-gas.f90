program main
  use assertions_interface, only : assert
  use gas_module, only :  gas_t, define, get_c_p, get_MW, R_gas, c_v, g
  use kind_parameters, only : DP
  implicit none
  type (gas_t) gas
  real(DP), parameter :: c_p_expected=1000.E0_DP, MW_expected=2.8E1_DP
  real(DP), parameter :: tolerance=1.E-6_DP, R_universal=8314_DP

  call define(gas, "volfil.inp")

  test_getters: &
  associate(c_p => get_c_p(gas), MW => get_MW(gas))

    call assert(abs((c_p - c_p_expected)/c_p_expected) <= tolerance, &
               "abs((c_p - c_p_expected)/c_p_expected) <= tolerance)")
    call assert(abs((MW  - MW_expected )/MW_expected ) <= tolerance, &
               "abs((MW  - MW_expected )/MW_expected ) <= tolerance)")

    test_procedures: &
    associate(R_gas_expected => R_universal/MW)

      call assert(abs(R_gas(gas) - R_gas_expected )/R_gas_expected <= tolerance, &
                 "abs(R_gas(gas) - R_gas_expected )/R_gas_expected <= tolerance")

      associate(c_v_expected => c_p_expected - R_gas_expected)

        call assert(abs(c_v(gas) - c_v_expected )/c_v_expected <= tolerance, &
                   "abs(c_v(gas) - c_v_expected )/c_v_expected <= tolerance")

        associate( g_expected=> c_p_expected/c_v_expected)

          call assert(abs(g(gas) - g_expected )/g_expected <= tolerance, &
                     "abs(g(gas) - g_expected )/g_expected <= tolerance")

        end associate
      end associate
    end associate test_procedures

  end associate test_getters

  print *, "Test passed"
end program
