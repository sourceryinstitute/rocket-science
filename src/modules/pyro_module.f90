module pyro_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : DP
  implicit none
  private

  public :: pyro_t
  public :: define
  public :: gen_height
  public :: gen_dia
  public :: gen_mass
  public :: rho_solid
  public :: ntabs

  type pyro_t
    private
    real(DP) T_flame, m_pkg, gen_mass, gen_height, gen_dia, rho_solid, r_ref, n
  end type

  interface define
    module procedure define_pyro
  end interface

contains

  function ntabs(this) result(num_tablets)
    use math_constants, only : pi
    type(pyro_t), intent(in) :: this
    real(DP) num_tablets

    associate(voltab => this%gen_height*pi*0.25_DP*this%gen_dia**2)
      associate(mtab => voltab*this%rho_solid)
        num_tablets = this%gen_mass/mtab
      end associate
    end associate
  end function

  subroutine define_pyro(this, input_file)
    type(pyro_t), intent(out) :: this
    character(len=*), intent(in) :: input_file
    character(len=max_errmsg_len) error_message
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    real(DP)       T_flame, m_pkg, gen_mass, gen_height, gen_dia, rho_solid, r_ref, n
    namelist/pyro/ T_flame, m_pkg, gen_mass, gen_height, gen_dia, rho_solid, r_ref, n

    open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "pyro%define: io_status == success ", diagnostic_data = error_message)
    read(file_unit, nml=pyro)
    close(file_unit)
    this%T_flame = T_flame
    this%m_pkg = m_pkg
    this%gen_mass = gen_mass
    this%gen_height = gen_height
    this%gen_dia = gen_dia
    this%rho_solid = rho_solid
    this%r_ref = r_ref
    this%n = n
  end subroutine

  function gen_dia(this) result(this_gen_dia)
    type(pyro_t), intent(in) :: this
    real(DP) this_gen_dia
    this_gen_dia = this%gen_dia
  end function

  function gen_height(this) result(this_gen_height)
    type(pyro_t), intent(in) :: this
    real(DP) this_gen_height
    this_gen_height = this%gen_height
  end function

  function gen_mass(this) result(this_gen_mass)
    type(pyro_t), intent(in) :: this
    real(DP) this_gen_mass
    this_gen_mass = this%gen_mass
  end function

  function rho_solid(this) result(this_rho_solid)
    type(pyro_t), intent(in) :: this
    real(DP) this_rho_solid
    this_rho_solid = this%rho_solid
  end function

end module pyro_module

!real(DP) :: m_gen, height, diameter, gas_yield, density, flame_temp, burn_rate_ref, burn_rate_exp
!real(DP) :: num_tablets, burn_dist, m_dot_gen, e_dot_gen
