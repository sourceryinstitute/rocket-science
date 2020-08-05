module combustion_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

  private
  public :: combustion_t

  type combustion_t
    private
    real(rkind) T_flame_
    real(rkind) m_pkg_
    real(rkind) gen_mass_
    real(rkind) gen_height_
    real(rkind) gen_dia_
    real(rkind) rho_solid_
    real(rkind) r_ref_
    real(rkind) n_
  contains
    procedure :: gen_height
    procedure :: gen_dia
    procedure :: gen_mass
    procedure :: rho_solid
    procedure :: m_pkg
    procedure :: ntabs
    procedure :: burn_rate
    procedure :: T_flame
  end type

  interface combustion_t
    module procedure construct_combustion_t
  end interface

contains

  function construct_combustion_t(input_file) result(new_combustion_t)
    !! Define this combustion object by reading values from an input file
    type(combustion_t)  new_combustion_t
    character(len=*), intent(in) :: input_file
    character(len=max_errmsg_len) error_message
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    real(rkind) T_flame, m_pkg, gen_mass, gen_height, gen_dia, rho_solid, r_ref, n
    namelist/combustion/ T_flame, m_pkg, gen_mass, gen_height, gen_dia, rho_solid, r_ref, n

    open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "combustion%define: io_status == success ", diagnostic_data = error_message)
    read(file_unit, nml=combustion)
    close(file_unit)

    new_combustion_t%T_flame_ = T_flame
    new_combustion_t%m_pkg_ = m_pkg
    new_combustion_t%gen_mass_ = gen_mass
    new_combustion_t%gen_height_ = gen_height
    new_combustion_t%gen_dia_ = gen_dia
    new_combustion_t%rho_solid_ = rho_solid
    new_combustion_t%r_ref_ = r_ref
    new_combustion_t%n_ = n
  end function

  pure function burn_rate(this, p) result(rate)
    !! Result is the rate of surface-normal depth loss
    class(combustion_t), intent(in) :: this
    real(rkind), intent(in) :: p
    real(rkind) rate
    real(rkind), parameter :: p_ref = 20.7E6_rkind  !! reference pressure
    rate = this%r_ref_*(p/p_ref)**this%n_ ! (ref. rate) * (chamber pressure / ref. pressure)**(rate_exponent)
  end function

  pure function ntabs(this) result(num_tablets)
    !! Result is the number of tablets
    use universal_constants, only : pi
    class(combustion_t), intent(in) :: this
    real(rkind) num_tablets

    associate(voltab => this%gen_height_*pi*0.25_rkind*this%gen_dia_**2)
      associate(mtab => voltab*this%rho_solid_)
        num_tablets = this%gen_mass_/mtab
      end associate
    end associate
  end function

  pure function gen_dia(this) result(this_gen_dia)
    class(combustion_t), intent(in) :: this
    real(rkind) this_gen_dia
    this_gen_dia = this%gen_dia_
  end function

  pure function gen_height(this) result(this_gen_height)
    class(combustion_t), intent(in) :: this
    real(rkind) this_gen_height
    this_gen_height = this%gen_height_
  end function

  pure function gen_mass(this) result(this_gen_mass)
    class(combustion_t), intent(in) :: this
    real(rkind) this_gen_mass
    this_gen_mass = this%gen_mass_
  end function

  pure function rho_solid(this) result(this_rho_solid)
    class(combustion_t), intent(in) :: this
    real(rkind) this_rho_solid
    this_rho_solid = this%rho_solid_
  end function

  pure function m_pkg(this) result(this_m_pkg)
    class(combustion_t), intent(in) :: this
    real(rkind) this_m_pkg
    this_m_pkg = this%m_pkg_
  end function

  pure function T_flame(this) result(this_T_flame)
    class(combustion_t), intent(in) :: this
    real(rkind) this_T_flame
    this_T_flame = this%T_flame_
  end function

end module combustion_module
