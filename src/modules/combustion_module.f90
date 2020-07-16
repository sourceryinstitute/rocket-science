module combustion_module
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : DP
  implicit none
  private

  public :: combustion_t
  public :: define
  public :: gen_height
  public :: gen_dia
  public :: gen_mass
  public :: rho_solid
  public :: ntabs
  public :: m_dot_gen
  public :: e_dot_gen

  type combustion_t
    private
    real(DP) T_flame, m_pkg, gen_mass, gen_height, gen_dia, rho_solid, r_ref, n
  end type

  interface define
    module procedure define_combustion
  end interface

  interface m_dot_gen
    module procedure m_dot_gen_combustion
  end interface

contains

  function burn_rate(this, p) result(rate)
    !! Result is the mass burn rate
    type(combustion_t), intent(in) :: this
    real(DP), intent(in) :: p                 !! pressure
    real(DP) rate
    real(DP), parameter :: p_ref = 20.7E6_DP  !! reference pressure
    rate = this%r_ref*(p/p_ref)**this%n ! (ref. rate) * (chamber pressure / ref. pressure)**(rate_exponent)
  end function

  function m_dot_gen_combustion(this, MW, p, dt, burn_depth) result(m_dot)
    !! Result is the mass loss rate
    use math_constants, only : pi
    type(combustion_t), intent(in) :: this
    real(DP), intent(in) :: MW !! molecular weight
    real(DP), intent(in) :: p  !! gas pressure
    real(DP), intent(in) :: dt !! time step
    real(DP), intent(in) :: burn_depth !! surface-normal burn depth
    real(DP) m_dot             !! mass loss rate

    !associate(r => 0.5*this%gen_dia, h => (this%gen_height)) ! original radius & height
    !  associate(br => burn_rate(this,p))
    !    associate(dn => br*dt)
    !     del_n = del_n + dn !! cumulative surface-normal burn distance
    !     associate(surface => merge(0._DP, ntabs(this) * (2*pi*(r-del_n)*(h-2*del_n) + 2*pi*(r-del_n)**2), any(del_n > [r, h/2])))
    !       !                { 0 if dn exceeds tablet thickness radially (measured from axis of symmetry)
    !       ! surface area = { 0 if dn exceeds tablet thickness axially (measured from center)
    !       !                { # tablets * (area of cylinder shrunken by dn in all directions) otherwise
    !      m_dot = br*surface*this%rho_solid     ! mass release rate = burn rate x surface area x density
    !      m_dot = m_dot*this%m_pkg*MW/1000._DP  ! amount of gas created vs solids, factoring in the gas yield
    !     end associate
    !   end associate
    !end associate
  end function

  function e_dot_gen(this, m_dot, c_p) result(e_dot)
    !! Result is the enthalpy flow rate
    type(combustion_t), intent(in) :: this
    real(DP), intent(in) :: m_dot !! mass liberation rate
    real(DP), intent(in) :: c_p   !! specific heat at constant pressure
    real(DP) e_dot                !! internal energy release rate
    e_dot = m_dot*c_p*this%T_flame
  end function

  function ntabs(this) result(num_tablets)
    !! Result is the number of tablets
    use math_constants, only : pi
    type(combustion_t), intent(in) :: this
    real(DP) num_tablets

    associate(voltab => this%gen_height*pi*0.25_DP*this%gen_dia**2)
      associate(mtab => voltab*this%rho_solid)
        num_tablets = this%gen_mass/mtab
      end associate
    end associate
  end function

  subroutine define_combustion(this, input_file)
    !! Define this combustion object by reading values from an input file
    type(combustion_t), intent(out) :: this
    character(len=*), intent(in) :: input_file
    character(len=max_errmsg_len) error_message
    integer :: io_status, file_unit
    integer, parameter :: success = 0
    real(DP)       T_flame, m_pkg, gen_mass, gen_height, gen_dia, rho_solid, r_ref, n
    namelist/combustion/ T_flame, m_pkg, gen_mass, gen_height, gen_dia, rho_solid, r_ref, n

    open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
    call assert(io_status == success, "combustion%define: io_status == success ", diagnostic_data = error_message)
    read(file_unit, nml=combustion)
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
    type(combustion_t), intent(in) :: this
    real(DP) this_gen_dia
    this_gen_dia = this%gen_dia
  end function

  function gen_height(this) result(this_gen_height)
    type(combustion_t), intent(in) :: this
    real(DP) this_gen_height
    this_gen_height = this%gen_height
  end function

  function gen_mass(this) result(this_gen_mass)
    type(combustion_t), intent(in) :: this
    real(DP) this_gen_mass
    this_gen_mass = this%gen_mass
  end function

  function rho_solid(this) result(this_rho_solid)
    type(combustion_t), intent(in) :: this
    real(DP) this_rho_solid
    this_rho_solid = this%rho_solid
  end function

end module combustion_module
