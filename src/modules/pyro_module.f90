module pyro_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : DP
  implicit none
  private

  public :: pyro_t
  public :: define

  type pyro_t
    private
    real(DP) T_flame, m_pkg, gen_mass, gen_height, gen_dia, rho_solid, r_ref, n
    !real(DP) :: m_gen, height, diameter, gas_yield, density, flame_temp, burn_rate_ref, burn_rate_exp
    !real(DP) :: num_tablets, burn_dist, m_dot_gen, e_dot_gen
  end type

  interface define
    module procedure define_pyro
  end interface

contains

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

end module pyro_module
