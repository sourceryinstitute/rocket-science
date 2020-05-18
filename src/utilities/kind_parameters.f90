module kind_parameters
  implicit none

  private
  public :: DP

  integer, parameter :: precision=15, range=307
  integer, parameter :: DP = selected_real_kind(precision, range)

end module
