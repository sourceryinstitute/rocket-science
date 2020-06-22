program maingas
  use gas_module, only :  c_p ,defineg,get_gas !, MW, T,m
  use kind_parameters, only : DP
  implicit none
  type (gas_t) gas
  !real, parameter :: expected_c_p=1000.E0, expected_MW=2.8E1,&
  !                   expected_T=3.E2, expected_m=0.03E0,tolerance=1.E-6 
  real, parameter :: expected_c_p=1000.0E0, tolerance= 1.E-6
  call defineg(gas, "volfil.inp")

  if( abs((get_gas(c_p) - expected_c_p)/expected_c_p) > tolerance) error stop "Test Failed. cp"
!  if( abs((get_gas(MW) - expected_MW)/expected_MW) > tolerance) error stop "Test Failed. mw"
!  if( abs((get_gas(T) - expected_T)/expected_T) > tolerance) error stop "Test Failed. T"
!  if( abs((get_gas(m) - expected_m)/expected_m) > tolerance) error stop "Test Failed. m"
  print *, "Test passed"
end program



program main
  use chamber_module, only : chamber_t, define, get_volume
  use kind_parameters, only : DP
  implicit none
  type(chamber_t) chamber
  real, parameter :: expected_volume=10.E-04_DP, tolerance=1.E-06

  call define(chamber, "volfil.inp")

  if ( abs((get_volume(chamber) - expected_volume)/expected_volume) > tolerance)  error stop "Test failed."

  print *,"Test passed."
end program

