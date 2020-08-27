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
    procedure :: thrust
  end type

contains

   subroutine define(this, input_file)
      class(nozzle_t), intent(out) :: this
      character(len=*), intent(in) :: input_file
      real(rkind) :: dia_ , C_f_
      namelist/nozzle_list/ dia_, C_f_

      block
        character(len=max_errmsg_len) error_message
        integer :: io_status, file_unit
        integer, parameter :: success = 0

        open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
        call assert(io_status == success, "nozzle_t%define: io_status == success ", diagnostic_data = error_message)
        read(file_unit, nml=nozzle_list)
        close(file_unit)
      end block

      this%diameter_ = dia_
      this%C_f_ = C_f_
   end subroutine

   pure function diameter(this)
     class(nozzle_t), intent(in) :: this
     real(rkind) diameter
     diameter = this%diameter_
   end function

   elemental function area(this)
     use universal_constants, only : pi
     class(nozzle_t), intent(in) :: this
     real(rkind) area
     area = pi*(this%diameter_**2)/4._rkind
   end function

   elemental function thrust(this, gage_pressure)
     class(nozzle_t), intent(in) :: this
     real(rkind), intent(in) :: gage_pressure
     real(rkind) thrust
     thrust = gage_pressure*this%area()*this%C_f_ ! correction to thrust (actual vs vacuum thrust)
   end function

end module nozzle_module
