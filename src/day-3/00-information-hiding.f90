module foo_module
  implicit none

  private
  public :: i
  public :: k

  integer :: i=1, j=2, k=3
end module

program main
  use foo_module, only : i
  implicit none

  print *,i
  print *,j
  print *,k

end program
