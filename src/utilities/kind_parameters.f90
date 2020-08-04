module kind_parameters
  use iso_fortran_env, only : real64
  !! select and expose kind parameters
  implicit none

  private
  public :: rkind

  integer, parameter :: precision=15, range=307
  integer, parameter :: rkind = selected_real_kind(precision, range)

  ! Alterntives:
  !
  ! real(real64) x     ! only specifies bit-representation, not numerical precision
  ! double precision y ! leaves wiggle room
  ! real(8)            ! behavior not defined by standard
end module
