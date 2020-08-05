module hole_module
  !! Encapsulate hole geometry and geometrical calculations
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind

  implicit none
  private

  public :: hole_t

  type hole_t
    private
    real(rkind) diameter_
  contains
    procedure :: diameter
    procedure :: area
  end type

  interface hole_t
    module procedure construct_hole_t
  end interface

contains

   function construct_hole_t(input_file) result(new_hole_t)
      character(len=*), intent(in) :: input_file
      type(hole_t) new_hole_t
      character(len=max_errmsg_len) error_message
      real(rkind) :: diameter
      integer :: io_status, file_unit
      integer, parameter :: success = 0
      namelist/hole/ diameter

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "hole%define: io_status == success ", diagnostic_data = error_message)
      read(file_unit, nml=hole)
      close(file_unit)

      new_hole_t%diameter_ = diameter
   end function

   pure function diameter(this) result(this_diameter)
     class(hole_t), intent(in) :: this
     real(rkind) this_diameter
     this_diameter = this%diameter_
   end function

   pure function area(this) result(hole_area)
     use universal_constants, only : pi
     class(hole_t), intent(in) :: this
     real(rkind) hole_area
     hole_area = pi*(this%diameter_**2)/4._rkind
   end function

end module hole_module