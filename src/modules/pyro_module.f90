module pyro_module
  use kind_parameters, only : DP 
  implicit none
  private

  public :: pyro_t
  public :: define
  public :: get_burn_rate_exp
  public :: get_gas_yield

  type pyro_t
       real(DP) :: mgen, height, diameter, gas_yield, density, flame_temp, burn_rate_ref, burn_rate_exp
       real(DP) :: num_tablets, burn_dist, mdotgen, edotgen
  end type

 contains

    subroutine define(this, file_name)
       use assertions_interface, only : max_errmsg_len
       type(pyro_t), intent(out) :: this
       character(len=*), intent(in) :: file_name
       character(len=max_errmsg_len) error_message
       real(DP) :: mgen, height, diameter, gas_yield, density, flame_temp, burn_rate_ref, burn_rate_exp
       integer :: io_status, file_unit
       integer, parameter :: success = 0
       namelist/pyro/ mgen, height, diameter, gas_yield, density, flame_temp, burn_rate_ref, burn_rate_exp
   
      open(newunit=file_unit, file=file_name, status="old", iostat=io_status, iomsg=error_message)
      if (io_status /= success) error stop "pyro%define:  "
      read(file_unit, nml=pyro)
      this%gas_yield = gas_yield
      this%burn_rate_exp=burn_rate_exp
      close(file_unit)
  
    end subroutine
  
    function get_gas_yield(this) result(this_gas_yield)
      type(pyro_t), intent(in) :: this
      real(DP) :: this_gas_yield
      this_gas_yield = this%gas_yield
    end function
  
    function get_burn_rate_exp(this) result(this_burn_rate_exp)
      type(pyro_t), intent(in) :: this
      real(DP) :: this_burn_rate_exp
      this_burn_rate_exp = this%burn_rate_exp
    end function
  end module pyro_module
