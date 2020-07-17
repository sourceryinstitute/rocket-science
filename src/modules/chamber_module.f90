module chamber_module
  use assertions_interface, only : assert, max_errmsg_len
  use gas_module, only : gas_t, define, c_v, R_gas, T, p, g, h
  use combustion_module, only : combustion_t, define, burn_rate, gen_height, gen_dia
  use hole_module, only : hole_t, define, area
  use kind_parameters, only : DP
  implicit none

  private
  public :: chamber_t
  public :: define
  public :: mass
  public :: energy
  public :: generate
  public :: efflux

  type chamber_t
    private
    real(DP) M, V
    type(gas_t) gas
    type(combustion_t) combustion
    type(hole_t) hole
  end type

  interface define
    module procedure define_chamber
  end interface

contains

  function mass(this) result(this_mass)
    type(chamber_t), intent(in) :: this
    real(DP) this_mass
    this_mass = this%M
  end function

  function energy(this) result(this_energy)
    type(chamber_t), intent(in) :: this
    real(DP) this_energy
    this_energy = this%M*c_v(this%gas)*T(this%gas)
  end function

  function generate(this, depth) result(generation_rate)
    use generation_rate_module, only : generation_rate_t, define
    use universal_constants, only : pi
    type(chamber_t), intent(in) :: this
    type(generation_rate_t) generation_rate
    real(DP), intent(in) :: depth

   !associate(br => dt*burn_rate(this%combustion, p(this%gas, mass=this%M, volume=this%V)))
   !  associate(dn => dt*br)
   !   associate( &
   !     r => 0.5*gen_dia(this%combustion), & ! original radius
   !     h => gen_height(this%combustion),       & ! original height
   !     depth = depth + dn                & ! cumulative surface-normal burn distance
   !   )
   !     associate(surface => merge(0._DP, ntabs(this)*2*pi*((r-delta_sn)*(h-2*delta_sn) + (r-delta_sn)**2), any(del_n > [r, h/2])))
   !        !                { 0 if dn exceeds tablet thickness radially (measured from axis of symmetry)
   !        ! surface area = { 0 if dn exceeds tablet thickness axially (measured from center)
   !        !                { # tablets * (area of cylinder shrunken by dn in all directions) otherwise
   !       associate(m_dot => (br*surface*this%rho_solid) * (this%m_pkg*MW/1000._DP  )) !! (burn rate * area * density) *  gas yield
   !         associate(e_dot => m_dot*c_p(this%gas)*T_flame(this%combustion))
   !           call define(generation_rate, delta_sn = dn, mass_gen_rate = m_dot , energy_gen_rate = e_dot)
   !      end associate
   !    end associate
   !  end associate
   !end associate
  end function

  function efflux(this) result(flow_rate)
    use universal_constants, only : atmospheric_pressure
    use flow_rate_module, only : flow_rate_t, define
    type(chamber_t), intent(in) :: this
    type(flow_rate_t) flow_rate
    real(dp) mdtx

    associate( &
      gx => g(this%gas), &
      px => p(this%gas, mass = this%M, volume = this%V), &
      tx => T(this%gas), &
      rx => R_gas(this%gas), &
      ax => area(this%hole) &
    )
      associate( &
        p_ratio => px/atmospheric_pressure, &
        p_crit  => (2._DP/(gx+1._DP))**(gx/(gx-1._DP)) &
       )
       call assert(p_ratio <= 1._DP, "p_ratio <= 1._DP") ! assert positive flow

       associate(choked_flow => (1._DP / p_ratio) < p_crit)
         if (choked_flow) then
           associate(cstar => sqrt((1._DP / gx) * ((gx + 1._DP) / 2._DP) ** ((gx + 1._DP) / (gx - 1._DP)) * rx * tx))
             mdtx = px * ax / cstar
           end associate
         else
           associate(facx => p_ratio ** ((gx - 1._DP) / gx))
             associate(term1 => sqrt(gx * rx * tx / facx), term2 => sqrt((facx - 1._DP) / (gx - 1._DP)))
               mdtx = SQRT(2._DP) * px / p_ratio / rx / tx * facx * term1 * term2 * ax
             end associate
           end associate
         endif
       end associate
     end associate
    end associate

    call define(flow_rate, mass_outflow_rate = mdtx, energy_outflow_rate = mdtx*h(this%gas))
  end function efflux

  subroutine define_chamber(this, input_file)
    type(chamber_t), intent(out) :: this
    character(len=*), intent(in) :: input_file
    real(DP) :: volume, mass
    namelist/chamber/ volume, mass

    block
      integer, parameter :: success = 0
      character(len=max_errmsg_len) error_message
      integer :: io_status, file_unit

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "chamber%define: io_status == success", diagnostic_data = error_message)
      read(file_unit, nml=chamber)
      close(file_unit)
    end block

    this%V = volume
    this%M = mass

    call define(this%gas, input_file)
    call define(this%combustion, input_file)
    call define(this%hole, input_file)
  end subroutine

end module chamber_module
