module math_constants
  use kind_parameters, only : DP
  implicit none

  private
  public :: pi

  real(DP), parameter :: pi=3.141592654_DP
end module
