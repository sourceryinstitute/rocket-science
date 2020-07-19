module universal_constants
  use kind_parameters, only : DP
  implicit none

  private
  public :: pi
  public :: atmospheric_pressure

  real(DP), parameter :: pi = 3.141592654_DP
  real(DP), parameter :: atmospheric_pressure = 101325._DP ! Pascals

end module
