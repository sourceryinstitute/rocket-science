module chamber_module
  !! Encapsulate the chamber components: propellant grain, combustion model, gas, & nozzle geometry
  use assertions_interface, only : assert, max_errmsg_len
  use gas_module, only : gas_t
  use combustion_module, only : combustion_t
  use nozzle_module, only : nozzle_t
  use kind_parameters, only : rkind
  implicit none

  private
  public :: chamber_t

  type grain_t
    !! Encapsulate propellent abstraction as a friend of chamber_t
    real(rkind) id_, od_, length_, rho_solid_
  contains
    procedure :: surface_area
    procedure :: volume
  end type

  type chamber_t
    private
    type(grain_t) grain_
    type(gas_t) gas_
    type(combustion_t) combustion_
    type(nozzle_t) nozzle_
  contains
    procedure :: define
    procedure :: gas
    procedure :: initial_volume
    procedure :: efflux
    procedure :: generate
    procedure :: burn_rate
  end type

contains

  pure function surface_area(this, burn_depth) result(grain_area)
    use universal_constants, only : pi
    class(grain_t), intent(in) :: this
    real(rkind), intent(in) :: burn_depth
    real(rkind) grain_area
    integer, parameter :: ends = 2

    associate(lost_length => ends*burn_depth)
      associate( &
        grain_length => this%length_ - lost_length, &
        id => this%id_+ lost_length, &
        od => (this%od_) &
      )
        grain_area = merge(0._rkind, pi*(id*grain_length + od*lost_length), id>od .or. grain_length<0._rkind)
      end associate
    end associate
  end function

  subroutine define(this, input_file)
    !! Set all chamber components
    class(chamber_t), intent(out) ::  this
    character(len=*), intent(in) :: input_file
    real(rkind) :: id, od, length, rho_solid
    namelist/chamber/ id, od, length, rho_solid

    block
      integer, parameter :: success = 0
      character(len=max_errmsg_len) error_message
      integer :: io_status, file_unit

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "chamber_t%define: io_status == success", diagnostic_data = error_message)
      read(file_unit, nml=chamber)
      close(file_unit)
    end block

    this%grain_ = grain_t(id_=id, od_=od, length_=length, rho_solid_=rho_solid)

    call this%gas_%define(input_file)
    call this%combustion_%define(input_file)
    call this%nozzle_%define(input_file)
  end subroutine

  pure function gas(this) result(this_gas)
    class(chamber_t), intent(in) :: this
    type(gas_t) this_gas
    this_gas = this%gas_
  end function

  pure function volume(this, burn_depth) result(this_volume)
    use universal_constants, only : pi
    class(grain_t), intent(in) :: this
    real(rkind), intent(in) :: burn_depth
    real(rkind) this_volume
    integer, parameter :: ends = 2

    associate( &
      lost_length => ends*burn_depth, &
      ir => this%id_/2 + burn_depth, &
      or => this%od_/2 &
    )
      this_volume = pi*(ir**2 * (this%length_ - lost_length) + or**2 * lost_length)
    end associate
  end function

  pure function initial_volume(this) result(this_volume)
    class(chamber_t), intent(in) :: this
    real(rkind) this_volume
    this_volume = this%grain_%volume(burn_depth=0._rkind)
  end function

  pure function burn_rate(this) result(this_burn_rate)
    !! Result is the rate of surface-normal depth loss for the burning tablets
    class(chamber_t), intent(in) :: this
    real(rkind) this_burn_rate
    !this_burn_rate = this%combustion_%burn_rate(this%gas_%p())
  end function

  pure function generate(this, depth) result(rate)
    !! Result contains the burn rate, mass generation rate, and energy generation rate
    use generation_rate_module, only : generation_rate_t
    class(chamber_t), intent(in) :: this
    type(generation_rate_t) rate
    real(rkind), intent(in) :: depth

    associate( &
      r => this%burn_rate(), &
      A => this%grain_%surface_area(depth),  &
      rho => this%combustion_%rho_solid() &
    )
      associate( &
        m_dot => r*A*rho, &
        T_flame => this%combustion_%T_flame() &
      )
        associate(h => this%gas_%h( T_flame ))
          rate = generation_rate_t( &
            burn_rate = r, &
            mass_generation_rate = m_dot, &
            enthalpy_generation_rate = m_dot*h &
          )
        end associate
      end associate
    end associate
  end function

  pure function efflux(this) result(rate)
    !! Result contains the flow rates of mass and energy exiting the chamber through the nozzle
    use universal_constants, only : atmospheric_pressure
    use flow_rate_module, only : flow_rate_t
    class(chamber_t), intent(in) :: this
    type(flow_rate_t) rate
    real(rkind) mdtx
    real(rkind), parameter :: T_ambient=300._rkind

    !associate( &
    !  ax => this%nozzle_%area(), &
    !  gx => this%gas_%g(), &
    !  rx => this%gas_%R_gas() &
    !  px => this%gas_%p(), &
    !  tx => merge(this%gas_%T(), p1>p2) &
    !)
    !  associate( &
    !    p_ratio => px/atmospheric_pressure, &
    !    p_crit  => (2._rkind/(gx+1._rkind))**(gx/(gx-1._rkind)) &
    !   )
    !   associate(choked_flow => (1._rkind / p_ratio) < p_crit)
    !     if (choked_flow) then
    !       associate(cstar => sqrt((1._rkind / gx) * ((gx + 1._rkind) / 2._rkind) ** ((gx + 1._rkind) / (gx - 1._rkind)) * rx * tx))
    !         mdtx = px * ax / cstar
    !       end associate
    !     else
    !       associate(facx => p_ratio ** ((gx - 1._rkind) / gx))
    !         associate(term1 => sqrt(gx * rx * tx / facx), term2 => sqrt((facx - 1._rkind) / (gx - 1._rkind)))
    !           mdtx = SQRT(2._rkind) * px / p_ratio / rx / tx * facx * term1 * term2 * ax
    !         end associate
    !       end associate
    !     endif
    !   end associate
    ! end associate
    !end associate

    !rate = flow_rate_t(mass_outflow_rate = mdtx, energy_outflow_rate = mdtx*this%gas_%h())
  end function efflux

end module chamber_module
