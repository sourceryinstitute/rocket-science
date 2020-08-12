module persistent_state_module
  !! Encapsulate state variables that must persist throughught execution for purposes of accumulation
  use assertions_interface, only : assert, max_errmsg_len
  use kind_parameters, only : rkind
  implicit none

  private
  public :: persistent_state_t

  type persistent_state_t
    private
    real(rkind) mass_       !! mass contained in chamber
    real(rkind) energy_     !! internal energy contained in chamber
    real(rkind) burn_depth_ !! surface-normal burn distance
    real(rkind) time_       !! simulated time
  contains
    procedure :: define
    procedure :: energy
    procedure :: mass
    procedure :: burn_depth
    procedure :: time
    procedure :: add
    generic :: operator(+) => add
  end type

  interface persistent_state_t
    module procedure construct_state
  end interface

contains

  pure function construct_state(mass, energy, burn_depth, time) result(new_state)
    real(rkind), intent(in) ::  mass, energy, burn_depth, time
    type(persistent_state_t) new_state

    new_state%mass_ = mass
    new_state%energy_ = energy
    new_state%burn_depth_= burn_depth
    new_state%time_ = time
  end function

  subroutine define(this, input_file, gas, volume, time, burn_depth)
    use gas_module, only : gas_t
    !! result is a newly constructured persistent_state_t object
    class(persistent_state_t), intent(out) :: this
    character(len=*), intent(in) :: input_file
    type(gas_t), intent(in) :: gas
    real(rkind), intent(in) :: volume, time, burn_depth
    real(rkind) :: temperature, pressure
    namelist/persistent_state/ temperature, pressure

    this%time_ = time
    this%burn_depth_ = burn_depth

    block
      integer, parameter :: success = 0
      character(len=max_errmsg_len) error_message
      integer :: io_status, file_unit

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "persistent_state_t%define: io_status == success", diagnostic_data = error_message)
      read(file_unit, nml=persistent_state)
      close(file_unit)
    end block

    associate(V => (volume), T => (temperature), p => (pressure))
      associate(R_gas => gas%R_gas(), c_v => gas%c_v())
        this%mass_ = p*V/(R_gas*T)
        this%energy_ = c_v*T
      end associate
    end associate
  end subroutine

  pure function time(this) result(this_time)
    !! get the time state variable
    class(persistent_state_t), intent(in) :: this
    real(rkind) this_time
    this_time = this%time_
  end function

  pure function burn_depth(this) result(this_burn_depth)
    !! get the burn_depth state variable
    class(persistent_state_t), intent(in) :: this
    real(rkind) this_burn_depth
    this_burn_depth = this%burn_depth_
  end function

  pure function energy(this) result(this_energy)
    !! get the energy state variable
    class(persistent_state_t), intent(in) :: this
    real(rkind) this_energy
    this_energy = this%energy_
  end function

  pure function mass(this) result(this_mass)
    !! get the mass state variable
    class(persistent_state_t), intent(in) :: this
    real(rkind) this_mass
    this_mass = this%mass_
  end function

  pure function add(lhs, rhs) result(total)
    !! result has components computed from summing lhs & rhs components
    class(persistent_state_t), intent(in) :: lhs, rhs
    type(persistent_state_t) total
    total%time_       = lhs%time_       + rhs%time_
    total%burn_depth_ = lhs%burn_depth_ + rhs%burn_depth_
    total%mass_       = lhs%mass_       + rhs%mass_
    total%energy_     = lhs%energy_    + rhs%energy_
  end function

end module
