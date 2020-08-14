module combustion_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

  private
  public :: combustion_t

  type combustion_t
    private
    real(rkind) T_flame_    !! adiabatic flame temperature
    real(rkind) rho_solid_  !! solid density
    real(rkind) r_ref_      !! reference burn rate
    real(rkind) n_          !! burn-rate exponent
  contains
    procedure :: define
    procedure :: T_flame
    procedure :: rho_solid
    procedure :: burn_rate
  end type

contains

  subroutine define(this, input_file)
    !! Define this combustion object by reading values from an input file
    class(combustion_t), intent(out) ::  this
    character(len=*), intent(in) :: input_file
    real(rkind) T_flame, rho_solid, r_ref, n
    namelist/combustion/ T_flame, r_ref, n

    block
      integer :: io_status, file_unit
      integer, parameter :: success = 0
      character(len=max_errmsg_len) error_message

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "combustion%define: io_status == success ", diagnostic_data = error_message)
      read(file_unit, nml=combustion)
      close(file_unit)
    end block

    this%T_flame_ = T_flame
    this%r_ref_ = r_ref
    this%n_ = n
  end subroutine

  pure function burn_rate(this, p)
    !! Result is the rate of surface-normal depth loss
    class(combustion_t), intent(in) :: this
    real(rkind), intent(in) :: p
    real(rkind) burn_rate
    real(rkind), parameter :: p_ref = 3000._rkind*6894.76  !! reference pressure 3000 psia converted to pascals
    burn_rate = this%r_ref_*(p/p_ref)**this%n_ ! (ref. rate) * (chamber pressure / ref. pressure)**(rate_exponent)
  end function

  pure function rho_solid(this)
    class(combustion_t), intent(in) :: this
    real(rkind) rho_solid
    rho_solid = this%rho_solid_
  end function

  pure function T_flame(this)
    class(combustion_t), intent(in) :: this
    real(rkind) T_flame
    T_flame = this%T_flame_
  end function

end module combustion_module
