module hole_module
  !! Encapsulate hole geometry and geometrical calculations
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind

  implicit none
  private

  public :: hole_t
  public :: define
  public :: diameter
  public :: area

  type hole_t
    private
    real(rkind) diameter
  end type

  interface define
    module procedure define_hole
  end interface

  interface diameter
    module procedure hole_diameter
  end interface

contains

   subroutine define_hole(this, input_file)
      type(hole_t), intent(out) :: this
      character(len=*), intent(in) :: input_file
      character(len=max_errmsg_len) error_message
      real(rkind) :: diameter
      integer :: io_status, file_unit
      integer, parameter :: success = 0
      namelist/hole/ diameter

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "hole%define: io_status == success ", diagnostic_data = error_message)
      read(file_unit, nml=hole)
      close(file_unit)
      this%diameter = diameter
   end subroutine

   function hole_diameter(this) result(this_diameter)
     type(hole_t), intent(in) :: this
     real(rkind) this_diameter
     this_diameter = this%diameter
   end function

   function area(this) result(hole_area)
     use universal_constants, only : pi
     type(hole_t), intent(in) :: this
     real(rkind) hole_area
     hole_area = pi*(this%diameter**2)/4._rkind
   end function

end module hole_module
