module hole_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : DP

  implicit none
  real(DP), parameter :: pi=3.1415916539_DP
  private

  public :: hole_t
  public :: define
  public :: get_diameter
  public :: area

  type hole_t
    private
    real(DP) diameter
  end type

  interface define
    module procedure define_hole
  end interface

contains

   subroutine define_hole(this, input_file)
      type(hole_t), intent(out) :: this
      character(len=*), intent(in) :: input_file
      character(len=max_errmsg_len) error_message
      real(DP) :: diameter
      integer :: io_status, file_unit
      integer, parameter :: success = 0
      namelist/hole_list/ diameter

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "hole%define: io_status == success ", diagnostic_data = error_message)
      read(file_unit, nml=hole_list)
      close(file_unit)
      this%diameter = diameter
   end subroutine

   function get_diameter(this) result(this_diameter)
     type(hole_t), intent(in) :: this
     real(DP) this_diameter

     this_diameter = this%diameter
   end function

   function area(this) result(hole_area)
     type(hole_t), intent(in) :: this
     real(DP) hole_area
     real(DP), parameter :: pi = 3.141592654_DP

     hole_area = pi*(this%diameter**2)/4.
   end function

end module hole_module
