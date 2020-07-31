module universal_constants
  use kind_parameters, only : rkind
  implicit none

  private
  public :: pi
  public :: atmospheric_pressure

  real(rkind), parameter :: pi = 3.141592654_rkind
  real(rkind), parameter :: atmospheric_pressure = 101325._rkind ! Pascals

end module
