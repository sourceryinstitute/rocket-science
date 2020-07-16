program main
  !! Test gas_module procedures
  use gas_module, only :  gas_t, define, c_p, MW, R_gas, c_v, g, h, e, T
    !! Type and procedures to be tested
  use assertions_interface, only : assert
  use kind_parameters, only : DP
  implicit none
  type (gas_t) gas
  character(len=*), parameter :: input_file = "volfil.inp"
  real(DP), parameter :: tolerance=1.E-6_DP, R_universal=8314_DP
  real(DP) c_p_expected, MW_expected, T_expected

  call read_test_data(input_file, c_p_expected, MW_expected, T_expected)

  call define(gas, input_file)

  call assert(abs((c_p(gas) - c_p_expected)/c_p_expected) <= tolerance, &
             "abs((c_p(gas) - c_p_expected)/c_p_expected) <= tolerance)")
  call assert(abs((MW(gas)  - MW_expected )/MW_expected ) <= tolerance, &
             "abs((MW(gas)  - MW_expected )/MW_expected ) <= tolerance)")
  call assert(abs((T(gas)  - T_expected )/T_expected ) <= tolerance, &
             "abs((T(gas)  - T_expected )/T_expected ) <= tolerance)")

  associate(h_expected => c_p(gas)*T(gas))
    call assert(abs((h(gas)  - h_expected )/h_expected ) <= tolerance, &
               "abs((h(gas)  - h_expected )/h_expected ) <= tolerance")
  end associate

  associate(e_expected => c_v(gas)*T(gas))
    call assert(abs((e(gas)  - e_expected )/e_expected ) <= tolerance, &
               "abs((e(gas)  - e_expected )/e_expected ) <= tolerance")
  end associate

  associate(R_gas_expected => R_universal/MW_expected)

    call assert(abs(R_gas(gas) - R_gas_expected )/R_gas_expected <= tolerance, &
               "abs(R_gas(gas) - R_gas_expected )/R_gas_expected <= tolerance")

    associate(c_v_expected => c_p_expected - R_gas_expected)

      call assert(abs(c_v(gas) - c_v_expected )/c_v_expected <= tolerance, &
                 "abs(c_v(gas) - c_v_expected )/c_v_expected <= tolerance")

      associate( g_expected => c_p_expected/c_v_expected)

        call assert(abs(g(gas) - g_expected )/g_expected <= tolerance, &
                   "abs(g(gas) - g_expected )/g_expected <= tolerance")

      end associate
    end associate
  end associate

  print *, "Test passed"

contains

  subroutine read_test_data(file_name, c_p, MW, temperature)
    use assertions_interface, only : assert, max_errmsg_len
    use kind_parameters, only : DP
    !! Read c_p & MW from namelist in the named file
    character(len=*), intent(in) :: file_name
    real(DP), intent(out) :: c_p, MW, temperature
    integer, parameter :: success = 0
    integer io_status, file_unit
    character(len=max_errmsg_len) error_message
    namelist/gas/ c_p, MW, temperature

    open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "gas read_test_data: io_status == success", diagnostic_data=error_message)
    read(file_unit, nml=gas)
    close(file_unit)
  end subroutine

end program
