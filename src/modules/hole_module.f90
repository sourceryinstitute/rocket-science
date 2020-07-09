module hole_module
  use kind_parameters, only : DP

  implicit none
  real(DP), parameter :: pi=3.1415916539_DP
  private

  public :: hole_t
  public :: define
  public :: get_diameter

  type hole_t
    private
    real(DP) :: diameter
  end type

contains

   subroutine define(this, file_name)
      use assertions_interface, only : max_errmsg_len
      type(hole_t), intent(out) :: this
      character(len=*), intent(in) :: file_name
      character(len=max_errmsg_len) error_message
      real(DP) :: diameter, area
      integer :: io_status, file_unit
      integer, parameter :: success = 0
      namelist/hole/ diameter

      open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
      if (io_status /= success) error stop "hole%define: file open failed with message "//error_message

      read(file_unit, nml=hole)
      close(file_unit)

      this%diameter=diameter

      print *, this%diameter

   end subroutine define

   function get_diameter(this) result(this_diameter)
     type(hole_t), intent(in) :: this
     real(DP) this_diameter
     this_diameter=this%diameter
   end function   

end module hole_module
