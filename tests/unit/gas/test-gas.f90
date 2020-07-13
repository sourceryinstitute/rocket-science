program main
  use assertions_interface, only : assert
  use gas_module, only :  gas_t, define, get_c_p, get_MW, get_T, get_m, R_gas, c_v, g
  use kind_parameters, only : DP
  implicit none
  type (gas_t) gas
  real(DP), parameter :: c_p_expected=1000.E0_DP, MW_expected=2.8E1_DP, T_expected=3.E2_DP, m_expected=0.03E0_DP
  real(DP), parameter :: tolerance=1.E-6_DP, R_universal=8.31446261815324_DP

  call define(gas, "volfil.inp")

  associate(c_p => get_c_p(gas), T => get_T(gas), MW => get_MW(gas), m => get_m(gas))
    call assert(abs((c_p - c_p_expected)/c_p_expected) <= tolerance, &
               "abs((c_p - c_p_expected)/c_p_expected) <= tolerance)")
    call assert(abs((T   - T_expected  )/T_expected  ) <= tolerance, &
               "abs((T   - T_expected  )/T_expected  ) <= tolerance)")
    call assert(abs((MW  - MW_expected )/MW_expected ) <= tolerance, &
               "abs((MW  - MW_expected )/MW_expected ) <= tolerance)")
    call assert(abs((m   - m_expected  )/m_expected  ) <= tolerance, &
               "abs((m   - m_expected  )/m_expected  ) <= tolerance)")
    associate(Rgas_expected => R_universal/MW)
      call assert(abs(R_gas(gas) - Rgas_expected )/Rgas_expected <= tolerance, &
                 "abs(R_gas(gas) - Rgas_expected )/Rgas_expected <= tolerance")
      associate(cv_expected => Rgas_expected - c_p)
        call assert(abs(c_v(gas) - cv_expected )/cv_expected <= tolerance, &
                   "abs(c_v(gas) - cv_expected )/cv_expected <= tolerance")
        associate( g_expected=> c_p/cv_expected)
          call assert(abs(g(gas) - g_expected )/g_expected <= tolerance, &
                     "abs(g(gas) - g_expected )/g_expected <= tolerance")
        end associate
      end associate
    end associate

  end associate

  print *, "Test passed"
end program
