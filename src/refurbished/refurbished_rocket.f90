module constants
  implicit none

  integer, parameter :: precision=15, range=307
  integer, parameter :: dp = selected_real_kind(precision, range)

  real(dp), parameter :: pi = 3.1415926539
  real(dp), parameter :: zero = 0._dp
  real(dp), parameter :: one = 1._dp
  real(dp), parameter :: p_amb = 101325d0 ! atmospheric pressure

end module

module burn_state_interface
  use constants, only : dp
  implicit none

  private

  type, public :: burn_state_t
    private
    real(dp) r_, db_
  contains
    procedure :: set_db
    procedure :: db
    procedure :: r
  end type

  interface burn_state_t
    module procedure new_burn_state_t
  end interface

  interface

    pure module function new_burn_state_t(old_burn_state, rref, p, n, dt)
      implicit none
      type(burn_state_t) :: new_burn_state_t
      type(burn_state_t), intent(in) :: old_burn_state
      real(dp), intent(in) :: rref, p, n, dt
    end function

    module subroutine set_db(this, db)
      implicit none
      class(burn_state_t), intent(inout) :: this
      real(dp), intent(in) :: db
    end subroutine

    pure module function db(this)
      implicit none
      class(burn_state_t), intent(in) :: this
      real(dp) db
    end function

    pure module function r(this)
      implicit none
      class(burn_state_t), intent(in) :: this
      real(dp) r
    end function

  end interface

end module burn_state_interface

submodule(burn_state_interface) burn_state_implementation
  implicit none
contains

  module procedure new_burn_state_t
    real(dp), parameter :: psipa=6894.76d0   ! unit conversion factor: pascals per psi
    real(dp), parameter :: pref=3000d0*psipa ! constant reference pressure for burn-rate calculation

    new_burn_state_t%r_ = rref*(p/pref)**n ! calculate burn rate
    associate(r => (new_burn_state_t%r_))
      new_burn_state_t%db_ = old_burn_state%db_+r*dt  ! calculate incremental burn distance
    end associate
  end procedure

  module procedure set_db
    this%db_ = db
  end procedure

  module procedure db
    db = this%db_
  end procedure

  module procedure r
    r = this%r_
  end procedure

end submodule burn_state_implementation

module geometry_interface
  !! propellent grain is a cylinder burning radially outward and axially inward.
  !! outer diameter is inhibited because this is a cast propellent: it was poured
  !! into the tube/chamber and only the inner diameter burns when ignited.
  use constants, only : dp
  implicit none

  private

  type, public :: geometry_t
    private
    real(dp) vol_, id_, od_, length_
  contains
    procedure :: surf
    procedure :: vol
    procedure :: burnout
  end type

  interface geometry_t
    module procedure new_geometry_t, incremented_geometry_t
  end interface

  interface

    pure module function new_geometry_t(vol, id, od, length)
      implicit none
      real(dp), intent(in) :: vol, id, od, length
      type(geometry_t) :: new_geometry_t
    end function

    pure module function incremented_geometry_t(old_geometry_t, volume_increment)
      !! cylinder burning from id outward and from both ends along the length
      implicit none
      type(geometry_t), intent(in) :: old_geometry_t
      real(dp), intent(in) :: volume_increment
      type(geometry_t) incremented_geometry_t
    end function

    pure module function surf(this, burn_depth)
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp), intent(in) :: burn_depth
      real(dp) surf
    end function

    pure module function vol(this)
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp) vol
    end function

    pure module function burnout(this, db)
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp), intent(in) :: db
      logical burnout
    end function

  end interface

end module geometry_interface

submodule(geometry_interface) geometry_implementation
  implicit none
contains

  module procedure new_geometry_t
    new_geometry_t%vol_    = vol
    new_geometry_t%id_     = id
    new_geometry_t%od_     = od
    new_geometry_t%length_ = length
  end procedure

  module procedure incremented_geometry_t
    incremented_geometry_t%id_     = old_geometry_t%id_
    incremented_geometry_t%od_     = old_geometry_t%od_
    incremented_geometry_t%length_ = old_geometry_t%length_
    incremented_geometry_t%vol_    = old_geometry_t%vol_ + volume_increment
  end procedure

  module procedure surf
    use constants, only : pi

    associate(db=>(burn_depth))
      associate(id=>(this%id_), od=>(this%od_), length=>(this%length_))
        surf = merge(0._dp, pi*(id+2.0d0*db)*(length-2.0d0*db)+0.5d0*pi*(od**2.0d0-(id+2.0*db)**2.0d0), this%burnout(db))
      end associate
    end associate

  end procedure

  module procedure burnout
    associate(id=>(this%id_), od=>(this%od_), length=>(this%length_))
      burnout = id+2*db>od .or. db>length/2
    end associate
  end procedure

  module procedure vol
    vol = this%vol_
  end procedure

end submodule geometry_implementation

module generation_rate_interface
  use constants, only : dp
  implicit none

  private

  type, public :: generation_rate_t
    private
    real(dp) mdotgen_, edotgen_
  contains
    procedure :: mdotgen
    procedure :: edotgen
  end type

  interface generation_rate_t
    module procedure calmdotgen
  end interface

  interface

    pure module function calmdotgen(rhos, r, surf, cp, Tflame) result(new_generation_rate)
      implicit none
      real(dp), intent(in) :: rhos, r, surf, cp, Tflame
      type(generation_rate_t) new_generation_rate
    end function

    pure module function mdotgen(this)
      implicit none
      class(generation_rate_t), intent(in) :: this
      real(dp) mdotgen
    end function

    pure module function edotgen(this)
      implicit none
      class(generation_rate_t), intent(in) :: this
      real(dp) edotgen
    end function

  end interface

end module generation_rate_interface

submodule(generation_rate_interface) generation_rate_implementation
  implicit none
contains

  module procedure calmdotgen
    associate(mdotgen => rhos*r*surf)
      new_generation_rate%mdotgen_ = mdotgen
      new_generation_rate%edotgen_ = mdotgen*cp*Tflame
    end associate
  end procedure

  module procedure edotgen
    edotgen = this%edotgen_
  end procedure

  module procedure mdotgen
    mdotgen = this%mdotgen_
  end procedure

end submodule generation_rate_implementation


module flow_rate_interface
  use constants, only : dp
  implicit none

  private

  type, public :: flow_rate_t
    private
    real(dp) mdotos_, edotos_
  contains
    procedure :: mdotos
    procedure :: edotos
  end type

  interface flow_rate_t
    module procedure massflow
  end interface

  interface

    pure module function massflow(t, g, rgas, p, cp, area) result(new_flow_rate)
       implicit none
       real(dp), intent(in) :: t, g, rgas, p, cp, area
       type(flow_rate_t) new_flow_rate
    end function

    pure module function mdotos(this)
      implicit none
      class(flow_rate_t), intent(in) :: this
      real(dp) mdotos
    end function

    pure module function edotos(this)
      implicit none
      class(flow_rate_t), intent(in) :: this
      real(dp) edotos
    end function

  end interface

end module flow_rate_interface

submodule(flow_rate_interface) flow_rate_implementation
  implicit none
contains

  module procedure massflow
    use constants, only : p_amb

    REAL (dp):: mdtx, tx,gx,rx,px,cpx,pcrit,facx,term1,term2,pratio,cstar,ax,hx, p1, p2, dsigng

    p1=p
    p2=p_amb
    ax=area

    IF(p1>p2) THEN
        dsigng=1
        tx=t
        gx=g
        rx=rgas
        px=p
        cpx=cp
        hx=cp*t
        pratio=p1/p2
     else
        dsigng=-1
        tx=300d0
        gx=g
        rx=rgas
        px=p_amb
        cpx=cp
        hx=cp*300d0
        pratio=p2/p1
    end if

    pcrit=(2./(gx+1.))**(gx/(gx-1.))

    associate(choked_flow => (1./pratio)<pcrit)
      calculate_mdtx: &
      IF(choked_flow) then
        cstar=sqrt((1./gx)*((gx+1.)/2.)**((gx+1.)/(gx-1.))*rx*tx)
        mdtx=px*ax/cstar
      else
        facx=pratio**((gx-1.)/gx)
        term1=SQRT(gx*rx*tx/facx)
        term2=SQRT((facx-1.)/(gx-1.))
        mdtx=SQRT(2.)*px/pratio/rx/tx*facx*term1*term2*ax
      end if calculate_mdtx
    end associate

    new_flow_rate%mdotos_ = mdtx*dsigng ! exiting mass flow (could be negative "dsigng")
    associate(engyx => mdtx*hx)  ! reformulate based on enthalpy of the chamber
      new_flow_rate%edotos_ = engyx*dsigng ! exiting enthalpy
    end associate

  end procedure

  module procedure edotos
    edotos = this%edotos_
  end procedure

  module procedure mdotos
    mdotos = this%mdotos_
  end procedure

end submodule flow_rate_implementation

module chamber_gas_interface
  use constants, only : dp
  implicit none

  private

  type, public :: chamber_gas_t
    private
    real(dp) MW_, c_p_, mcham_, echam_
  contains
    procedure :: R_gas
    procedure :: c_p
    procedure :: c_v
    procedure :: T
    procedure :: p => calcp
    procedure :: g
  end type

  interface chamber_gas_t
    module procedure new_chamber_gas_t, incremented_chamber_gas_t
  end interface

  interface

    pure module function new_chamber_gas_t(MW, c_p, T, p, V)
      implicit none
      real(dp), intent(in) :: MW, c_p, T, p, V
      type(chamber_gas_t) new_chamber_gas_t
    end function

    pure module function incremented_chamber_gas_t(old_chamber_gas_t, mass_increment, energy_increment)
      implicit none
      type(chamber_gas_t), intent(in) :: old_chamber_gas_t
      real(dp), intent(in) :: mass_increment, energy_increment
      type(chamber_gas_t) incremented_chamber_gas_t
    end function

    pure module function R_gas(this)
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) R_gas
    end function

    pure module function c_p(this)
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) c_p
    end function

    pure module function c_v(this)
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) c_v
    end function

    pure module function T(this)
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) T
    end function

    pure module function g(this)
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp) g
    end function

    pure module function calcp(this, V)
      implicit none
      class(chamber_gas_t), intent(in) :: this
      real(dp), intent(in) :: V
      real(dp) :: calcp
    end function

  end interface

end module chamber_gas_interface

submodule(chamber_gas_interface) chamber_gas_implementation
  implicit none
contains

  module procedure new_chamber_gas_t
    new_chamber_gas_t%MW_  = MW
    new_chamber_gas_t%c_p_ = c_p
    new_chamber_gas_t%mcham_ = p*V/(new_chamber_gas_t%R_gas()*T)
    new_chamber_gas_t%echam_  = new_chamber_gas_t%mcham_*new_chamber_gas_t%c_v()*T
  end procedure

  module procedure incremented_chamber_gas_t
    incremented_chamber_gas_t%MW_    = old_chamber_gas_t%MW_
    incremented_chamber_gas_t%c_p_   = old_chamber_gas_t%c_p_
    incremented_chamber_gas_t%mcham_ = old_chamber_gas_t%mcham_ + mass_increment
    incremented_chamber_gas_t%echam_ = old_chamber_gas_t%echam_ + energy_increment
  end procedure

  module procedure R_gas
    real(dp), parameter :: R_universal=8314d0
    R_gas = R_universal/this%MW_
  end procedure

  module procedure c_p
    real(dp), parameter :: R_universal=8314d0
    c_p = this%c_p_
  end procedure

  module procedure c_v
    c_v = this%c_p() - this%R_gas()
  end procedure

  module procedure T
    T = this%echam_/(this%mcham_*this%c_v())
  end procedure

  module procedure calcp
    calcp = this%mcham_*this%R_gas()*this%T()/V
  end procedure

  module procedure g
    g = this%c_p_/this%c_v()
  end procedure

end submodule chamber_gas_implementation

module nozzle_interface
  use constants, only : dp
  implicit none

  private

  type, public :: nozzle_t
    private
    real(dp) area_, C_f_
  contains
    procedure :: thrust => calcthrust
    procedure :: area
  end type

  interface nozzle_t
    module procedure new_nozzle_t
  end interface

  interface

    pure module function new_nozzle_t(dia, C_f)
      implicit none
      real(dp), intent(in) ::  dia, C_f
      type(nozzle_t) new_nozzle_t
    end function

    pure module function calcthrust(this, p)
      implicit none
      class(nozzle_t), intent(in) :: this
      real(dp), intent(in) :: p
      real(dp) calcthrust
    end function

    pure module function area(this)
      implicit none
      class(nozzle_t), intent(in) :: this
      real(dp) area
    end function

  end interface

end module nozzle_interface

submodule(nozzle_interface) nozzle_implementation
  implicit none
contains

  module procedure new_nozzle_t
    use constants, only : pi
    new_nozzle_t%area_ = pi/4d0*dia**2 ! nozzle area
    new_nozzle_t%C_f_ = C_f
  end procedure

  module procedure calcthrust
    use constants, only : p_amb
    calcthrust = (p-p_amb)*this%area_*this%C_f_ ! correction to thrust (actual vs vacuum thrust)
  end procedure

  module procedure area
    area = this%area_
  end procedure

end submodule nozzle_implementation

module refurbished_rocket_module
  implicit none

contains

function refurbished_rocket(input_file)
  !! this driver function simulates a single stage
  !! rocket motor flowing out of a nozzle, assuming
  !! a thrust coefficient and ignoring the complexities of
  !! what happens to thrust at low pressures, i.e. shock in the nozzle

use assertions_interface, only : assert, max_errmsg_len
use results_interface, only : results_t
use burn_state_interface, only : burn_state_t
use geometry_interface, only : geometry_t
use generation_rate_interface, only : generation_rate_t
use flow_rate_interface, only : flow_rate_t
use chamber_gas_interface, only : chamber_gas_t
use nozzle_interface, only : nozzle_t
use constants, only : dp, zero, one
implicit none

character(len=*), intent(in) :: input_file
type(results_t) refurbished_rocket

character(len=max_errmsg_len) error_message
integer io_status, file_unit
integer, parameter :: success = 0

type(burn_state_t) burn_state
type(geometry_t) geometry
type(chamber_gas_t) chamber_gas
type(nozzle_t) nozzle

real(dp), parameter :: initial_volume = one
real(dp) time, dt, tmax, rhos, Tflame
real(dp), allocatable :: output(:,:)

integer nsteps, i

real(dp) dt_, t_max_
real(dp) c_p_, MW_
real(dp) temperature_, pressure_
real(dp) T_flame_, r_ref_, n_
real(dp) id_, od_, length_, rho_solid_
real(dp) dia_, C_f_

namelist/numerics_list/ dt_, t_max_
namelist/gas_list/ c_p_, MW_
namelist/state_list/  temperature_, pressure_
namelist/combustion_list/ T_flame_, r_ref_, n_
namelist/grain_list/ id_, od_, length_, rho_solid_
namelist/nozzle_list/ dia_, C_f_

open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
call assert(io_status == success, "legcy_rocket: io_status == success", error_message)

read(file_unit, nml=numerics_list)
dt   = dt_
tmax = t_max_

read(file_unit, nml=gas_list)
read(file_unit, nml=state_list)
chamber_gas = chamber_gas_t(MW = MW_, c_p = c_p_, T = temperature_, p = pressure_, V = initial_volume)

read(file_unit, nml=combustion_list)
Tflame = T_flame_

read(file_unit, nml=grain_list)
geometry = geometry_t(vol = initial_volume, id = id_, od = od_, length = length_)
rhos   = rho_solid_

read(file_unit, nml=nozzle_list)
nozzle = nozzle_t(dia=dia_, C_f=C_f_)

close(file_unit)

call burn_state%set_db(zero) ! initialize propellant burn distance

nsteps=nint(tmax/dt) ! number of time steps

allocate(output(0:nsteps,6)) ! preallocate an output array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

  time = zero
  associate(mdotos => zero, thrust => zero, volume => geometry%vol())
    output(0,:)=[time, chamber_gas%p(volume), chamber_gas%T(), mdotos, thrust, volume]
  end associate

  associate(R_gas => chamber_gas%R_gas(), c_v => chamber_gas%c_v(), g => chamber_gas%g(), c_p => chamber_gas%c_p())

    do i=1,nsteps

      associate(p => chamber_gas%p(geometry%vol()))

        burn_state = burn_state_t(burn_state, r_ref_, p, n_, dt)

        associate(db => burn_state%db(), r => burn_state%r())
          associate(surf => geometry%surf(db))

            geometry = geometry_t(geometry,  merge(zero, r*surf*dt, geometry%burnout(db)) )

            associate( &
              flow_rate => flow_rate_t(chamber_gas%T(), g, R_gas, p, c_p, nozzle%area()), &
              generation_rate => generation_rate_t(rhos, r, surf, c_p, Tflame) &
            )
              associate(mdotos => flow_rate%mdotos())
                chamber_gas = chamber_gas_t( chamber_gas,  &
                  mass_increment   = (generation_rate%mdotgen() - mdotos)*dt, &
                  energy_increment = (generation_rate%edotgen() - flow_rate%edotos())*dt  &
                )
                associate(volume => geometry%vol())
                  associate(p => chamber_gas%p(volume))
                    time = time + dt
                    output(i,:)=[time, p, chamber_gas%T(), mdotos, nozzle%thrust(p), volume]
                  end associate
                end associate
              end associate
            end associate
          end associate
        end associate
      end associate
    end do
  end associate

  block
    character(len=*), parameter :: header(*) = [ character(len=len("temperatureRefurbished)")) :: &
      "timeRefurbished", "pressureRefurbished", "temperatureRefurbished", "mdotosRefurbished", "thrustRefurbished", &
      "volumeRefurbished"]
    refurbished_rocket = results_t(header, output)
  end block

end function refurbished_rocket

end module refurbished_rocket_module
