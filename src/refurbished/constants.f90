module constants
  !! Define universally constant mathematical and physical values
  implicit none

  private
  public :: dp, pi, zero, one, p_amb

  integer, parameter :: precision=15, range=307
  integer, parameter :: dp = selected_real_kind(precision, range)

  real(dp), parameter :: pi = 3.1415926539_dp
  real(dp), parameter :: zero = 0._dp
  real(dp), parameter :: one = 1._dp
  real(dp), parameter :: p_amb = 101325._dp ! atmospheric pressure
end module constants
