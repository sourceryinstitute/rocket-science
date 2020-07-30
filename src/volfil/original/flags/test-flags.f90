program test_flags
  use kind_parameters, only : DP
  use flags_module, only : flags, initialize, get_dt, get_tmax, set_dt, set_tmax
  implicit none

  real(DP), parameter :: tolerance = 1.0E-05_DP
  real(DP), parameter :: dt_expected=1.E-5_DP, tmax_expected=.1_DP
  type(flags) :: flag

  call initialize(flag, input_file='volfil.inp')

  if(abs(get_dt(flag) - dt_expected)/dt_expected > tolerance) error stop "test_flags: get_dt failed"
  if(abs(get_tmax(flag) - tmax_expected)/tmax_expected > tolerance) error stop "test_flags: get_tmax failed"

  call set_dt(flag, dt=2.*dt_expected)
  call set_tmax(flag, tmax=4.*tmax_expected)

  if(abs(get_dt(flag) - 2.*dt_expected)/(2.*dt_expected) > tolerance) error stop "test_flags: set_dt failed"
  if(abs(get_tmax(flag) - 4.*tmax_expected)/(4.*tmax_expected) > tolerance) error stop "test_flags: set_tmax failed"

  print *,"Test passed."

end program
