module nozzle_module
  !! Encapsulate nozzle geometry and geometrical calculations
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind

  implicit none
  private

  public :: nozzle_t

  type nozzle_t
    private
    real(rkind) diameter_
    real(rkind) C_f_        !! thrust coefficient
  contains
    procedure :: define
    procedure :: diameter
    procedure :: area
  end type

contains

   subroutine define(this, input_file)
      class(nozzle_t), intent(out) :: this
      character(len=*), intent(in) :: input_file
      real(rkind) :: diameter, C_f
      namelist/nozzle/ diameter, C_f

      block
        character(len=max_errmsg_len) error_message
        integer :: io_status, file_unit
        integer, parameter :: success = 0

        open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
        call assert(io_status == success, "nozzle_t%define: io_status == success ", diagnostic_data = error_message)
        read(file_unit, nml=nozzle)
        close(file_unit)
      end block

      this%diameter_ = diameter
      this%C_f_ = C_f
   end subroutine

   pure function diameter(this) result(this_diameter)
     class(nozzle_t), intent(in) :: this
     real(rkind) this_diameter
     this_diameter = this%diameter_
   end function

   pure function area(this) result(nozzle_area)
     use universal_constants, only : pi
     class(nozzle_t), intent(in) :: this
     real(rkind) nozzle_area
     nozzle_area = pi*(this%diameter_**2)/4._rkind
   end function

end module nozzle_module
