module chamber_module
  use assertions_interface, only : assert, max_errmsg_len
  use gas_module, only : gas_t, define, c_v, R_gas, T, p, g, h
  use combustion_module, only : combustion_t, define
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

  function generate(this) result(generation_rate)
    use generation_rate_module, only : generation_rate_t, define
    type(chamber_t), intent(in) :: this
    type(generation_rate_t) generation_rate

  !    combustion_model%r=comb%rref*(chamcond%p/pref)**comb%n ! forget about conditioning temperature for now
  !    ! burn rate =  (reference burn rate)(chamber pressure/reference pressure)**(burn rate exponent)

  !  !call define(generationw_rate, mass_generation_rate = , energy_generation_rate = )

  !contains

  !  subroutine calmdotgen(chamcond,comb,gp,flag)
  !  use flags_module, only : flags, get_dt
  !  type(gasprop),intent(in) :: gp
  !  type(chamber_internal), intent(in):: chamcond
  !  type(combustion), intent(inout) :: comb
  !  type(flags), intent(in) :: flag
  !  real(DP) :: surf,dist,h,r
  !  ! real(DP):: mdotgen, edotgen, tflame, mpkg, genmass, genheight, gendiam, rhosolid, ntabs, voltab, mtab,db,rref,r,n)


  !  ! Two object-oriented ways to set r using the formula above
  !  ! call set_r(comb, get_rref(comb)*(get_p(chamcond)/pref)**get_n(comb))
  !  ! call set_r(comb, get_r(comb,chamcond,pref))

  !  comb%db=comb%db+comb%r*get_dt(flag)
  !    ! cumulative burn distance = burn distance + burn rate x dt
  !  r=comb%gendiam/2.
  !    ! original radius
  !  dist=comb%db
  !    ! burn distance
  !  h=comb%genheight
  !    ! original height
  !  surf=comb%ntabs*(2*pi*(r-dist)*(h-2*dist)+2*pi*(r-dist)**2)
  !    ! num. tablets x (surface area of cylider shrunken by radial distance "dist")
  !  if(dist>r) surf=0.
  !    ! no tablet if burn distance exceeds radius
  !  if(dist>h/2) surf=0.
  !    ! no tablet if burn distance exceeds height of half vertically (burning from top and bottom)
  !  comb%mdotgen=comb%r*surf*comb%rhosolid ! amount of solid combusted
  !    ! mass generation rate = burn rate x surface area x density
  !  comb%summ=comb%summ+comb%mdotgen*get_dt(flag) ! keepting track of how much has burned
  !    ! cumulative mass burned
  !  if(comb%summ>comb%genmass) comb%mdotgen=0.
  !    ! if comulative mass burned exceeds original generant mass, burning stops
  ! ! now factor in the gas yield
  !  !1real(DP):: mdotgen, edotgen, tflame, mpkg, genmass, genheight, gendiam, rhosolid, ntabs, voltab, mtab,db,rref,r,n
  !  comb%mdotgen=comb%mdotgen*comb%mpkg*gp%mw/1d3 ! amount of gas created vs solids
  !    ! mass generation rate = mass burn rate x moles per kg * mol. weight / 1000. (mw = moles / g)
  !  comb%edotgen=comb%mdotgen*gp%cp*comb%tflame ! ENTHALPY
  !    ! enthalpy flow rate = mass generation rate * c_p * T
  !  end subroutine calmdotgen

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
