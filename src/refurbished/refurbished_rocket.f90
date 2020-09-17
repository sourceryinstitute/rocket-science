module global_variables
implicit none
save
integer, parameter :: precision=15, range=307
integer, parameter :: dp = selected_real_kind(precision, range)
real(dp), parameter :: pi=3.1415926539
real(dp), parameter :: zero=0._dp

real(dp):: dia,cf,rhos
real(dp):: Tflame
real(dp):: thrust=zero, area, n
real(dp):: texit, pamb,p
real(dp):: mcham,echam

end module

module burn_state_interface
  use global_variables, only : dp
  implicit none

  private

  type, public :: burn_state_t
    private
    real(dp) r_, db_
  contains
    procedure :: burnrate
    procedure :: set_db
    procedure :: set_r
    procedure :: db
    procedure :: r
  end type

  interface

    module subroutine burnrate(this, rref, p, n, dt)
      use global_variables, only : dp
      implicit none
      class(burn_state_t), intent(inout) :: this
      real(dp), intent(in) :: rref, p, n, dt
    end subroutine

    module subroutine set_db(this, db)
      use global_variables, only : dp
      implicit none
      class(burn_state_t), intent(inout) :: this
      real(dp), intent(in) :: db
    end subroutine

    module subroutine set_r(this, r)
      use global_variables, only : dp
      implicit none
      class(burn_state_t), intent(inout) :: this
      real(dp), intent(in) :: r
    end subroutine

    pure module function db(this)
      use global_variables, only : dp
      implicit none
      class(burn_state_t), intent(in) :: this
      real(dp) db
    end function

    pure module function r(this)
      use global_variables, only : dp
      implicit none
      class(burn_state_t), intent(in) :: this
      real(dp) r
    end function

  end interface

end module burn_state_interface

submodule(burn_state_interface) burn_state_implementation
  implicit none
contains

  module procedure burnrate
    real(dp), parameter :: psipa=6894.76d0   ! unit conversion factor: pascals per psi
    real(dp), parameter :: pref=3000d0*psipa ! constant reference pressure for burn-rate calculation

    this%r_ = rref*(p/pref)**n ! calculate burn rate
    associate(r => (this%r_))
      this%db_=this%db_+r*dt  ! calculate incremental burn distance
    end associate
    !print * , 'i,r_',i,r
  end procedure

  module procedure set_db
    this%db_ = db
  end procedure

  module procedure set_r
    this%r_ = r
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
  use global_variables, only : dp
  implicit none

  private

  type, public :: geometry_t
    private
    real(dp) vol_, id_, od_, length_
  contains
    procedure :: define
    procedure :: calcsurf
    procedure :: surf
    procedure :: vol
    procedure, private :: burnout
  end type

  interface

    module subroutine define(this, vol, id, od, length)
      use global_variables, only : dp
      implicit none
      class(geometry_t), intent(out) :: this
      real(dp), intent(in) :: vol, id, od, length
    end subroutine

    module subroutine calcsurf(this, burn_state, dt)
      !! cylinder burning from id outward and from both ends along the length
      use burn_state_interface, only : burn_state_t
      implicit none
      class(geometry_t), intent(inout) :: this
      type(burn_state_t), intent(inout) :: burn_state
      real(dp), intent(in) :: dt
    end subroutine

    pure module function surf(this, burn_depth)
      use global_variables, only : dp
      use burn_state_interface, only : burn_state_t
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp), intent(in) :: burn_depth
      real(dp) surf
    end function

    pure module function vol(this)
      use global_variables, only : dp
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp) vol
    end function

    pure module function burnout(this, db)
      use burn_state_interface, only : burn_state_t
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

  module procedure define
    this%vol_ = vol
    this%id_ = id
    this%od_ = od
    this%length_ = length
  end procedure

  module procedure calcsurf
    use global_variables, only : dp

    associate(db => burn_state%db())
      if(this%burnout(db)) call burn_state%set_r(0._dp)  ! turn off burn rate so burn distance stops increasing
      associate(r => burn_state%r(), surf => this%surf(db))
        this%vol_ = this%vol_ + r*surf*dt ! increment the interior volume of the chamber a little
      end associate
    end associate

  end procedure

  module procedure surf
    use global_variables, only : pi

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
  use global_variables, only : dp
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
  use global_variables, only : dp
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

    pure module function massflow(t, g, rgas, p, cp, pamb, area) result(new_flow_rate)
       implicit none
       real(dp), intent(in) :: t, g, rgas, p, cp, pamb, area
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

    REAL (dp):: mdtx, tx,gx,rx,px,cpx,pcrit,facx,term1,term2,pratio,cstar,ax,hx, p1, p2, dsigng

    p1=p
    p2=pamb
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
        px=pamb
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
  use global_variables, only : dp
  implicit none

  private

  type, public :: chamber_gas_t
    real(dp) MW_, c_p_, T_
  contains
    procedure :: define
    procedure :: R_gas
    procedure :: c_p
    procedure :: c_v
    procedure :: T
    procedure :: g
    procedure :: set_T => calct
  end type

  interface

    module subroutine define(this, MW, c_p, T)
      implicit none
      class(chamber_gas_t), intent(out) :: this
      real(dp), intent(in) :: MW, c_p, T
    end subroutine

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

    module subroutine calct(this, T)
      implicit none
      class(chamber_gas_t), intent(inout) :: this
      real(dp), intent(in) :: T
    end subroutine

  end interface

end module chamber_gas_interface

submodule(chamber_gas_interface) chamber_gas_implementation
  implicit none
contains

  module procedure define
    this%MW_  = MW
    this%c_p_ = c_p
    this%T_ = T
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
    T = this%T_
  end procedure

  module procedure g
    g = this%c_p_/this%c_v()
  end procedure

  module procedure calct
    this%T_ = T
  end procedure

end submodule chamber_gas_implementation

module refurbished_rocket_module
  implicit none

contains

subroutine addmass(mdotgen, edotgen, mdotos, edotos, dt)
    use global_variables, only : dp, mcham, echam
    implicit none
    real(dp), intent(in) :: mdotgen, edotgen, mdotos, edotos, dt
    mcham=mcham+(mdotgen-mdotos)*dt
    echam=echam+(edotgen-edotos)*dt
end subroutine


subroutine calcp(vol, rgas, T)
    use global_variables, only : dp, p, mcham
    implicit none
    real(dp), intent(in) :: vol, rgas, T
    p=mcham*rgas*T/vol
end subroutine

subroutine calcthrust
    use global_variables
    implicit none
    thrust=(p-pamb)*area*cf ! correction to thrust (actual vs vacuum thrust)
end subroutine


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
use global_variables
implicit none

character(len=*), intent(in) :: input_file
type(results_t) refurbished_rocket

character(len=max_errmsg_len) error_message
integer io_status, file_unit
integer, parameter :: success = 0

type(burn_state_t) burn_state
type(geometry_t) geometry
type(chamber_gas_t) chamber_gas

real(dp) :: time, dt, tmax
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

time = zero

read(file_unit, nml=numerics_list)
dt   = dt_
tmax = t_max_

read(file_unit, nml=gas_list)
read(file_unit, nml=state_list)
p = pressure_
call chamber_gas%define(MW_, c_p_, temperature_)

read(file_unit, nml=combustion_list)
Tflame = T_flame_
n      = n_

read(file_unit, nml=grain_list)
call geometry%define(vol = 1._dp, id = id_, od = od_, length = length_)
rhos   = rho_solid_

read(file_unit, nml=nozzle_list)
dia = dia_
cf  = C_f_

close(file_unit)

call burn_state%set_db(0._dp)   ! initialize propellant burn distance


nsteps=nint(tmax/dt) ! number of time steps

allocate(output(0:nsteps,6)) ! preallocate an output array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

  area=pi/4d0*dia**2.0d0 ! nozzle area

  pamb=101325d0 ! atmospheric pressure

  associate(V => geometry%vol(), mdotos => 0._dp, T=>chamber_gas%T(), c_v=>chamber_gas%c_v(), R_gas => chamber_gas%R_gas())
    output(0,:)=[time,p,T,mdotos,thrust,V]
    mcham = p*V/R_gas/T  ! use ideal gas law to determine mass in chamber
    echam = mcham*c_v*T                   ! initial internal energy in chamber
  end associate

  do i=1,nsteps
    call burn_state%burnrate(r_ref_, p, n, dt)
    call geometry%calcsurf(burn_state, dt)
    associate(R_gas => chamber_gas%R_gas(), c_v => chamber_gas%c_v(), g => chamber_gas%g(), c_p => chamber_gas%c_p())
      associate( &
        flow_rate => flow_rate_t(chamber_gas%T(), g, R_gas, p, c_p, pamb, area), &
        generation_rate => generation_rate_t(rhos, burn_state%r(), geometry%surf(burn_state%db()), chamber_gas%c_p(), Tflame) &
        )
          associate( &
            mdotos => flow_rate%mdotos(), &
            edotos => flow_rate%edotos(), &
            mdotgen => generation_rate%mdotgen(), &
            edotgen => generation_rate%edotgen() &
          )
          call addmass(mdotgen, edotgen, mdotos, edotos, dt)
          call chamber_gas%set_T(echam/mcham/c_v)
          associate(vol => geometry%vol(), T => chamber_gas%T())
            call calcp(vol, R_gas, T)
            call calcthrust
            time=time+dt
            output(i,:)=[time,p,T,mdotos,thrust,vol]
          end associate
        end associate
      end associate
    end associate
  enddo

  block
    character(len=*), parameter :: header(*) = [ character(len=len("temperatureRefurbished)")) :: &
      "timeRefurbished", "pressureRefurbished", "temperatureRefurbished", "mdotosRefurbished", "thrustRefurbished", &
      "volumeRefurbished"]
    refurbished_rocket = results_t(header, output)
  end block

end function refurbished_rocket

end module refurbished_rocket_module
