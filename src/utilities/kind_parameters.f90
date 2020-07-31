module kind_parameters
  !! select and expose kind parameters
  implicit none

  private
  public :: rkind

  integer, parameter :: precision=15, range=307
  integer, parameter :: rkind = selected_real_kind(precision, range)
end module
