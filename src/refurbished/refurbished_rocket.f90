module global_variables
implicit none
save
integer, parameter :: precision=15, range=307
integer, parameter :: dp = selected_real_kind(precision, range)
real(dp), parameter :: pi=3.1415926539
real(dp), parameter :: RU=8314d0
real(dp), parameter :: zero=0._dp, one=1._dp

real(dp):: cp,cv,g,rgas,mw,dia,cf,rref,rhos,psipa,pref
real(dp):: dt,tmax,Tflame
real(dp):: thrust=zero, area, n,mdotout,edotout,energy
real(dp):: mdotos=zero, edotos, texit, dsigng,pamb,p,t
real(dp):: mcham,echam,time=zero
integer nsteps,i
real(dp), allocatable :: output(:,:)

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

    module subroutine burnrate(this, rref, p, pref, n, dt)
      use global_variables, only : dp
      implicit none
      class(burn_state_t), intent(inout) :: this
      real(dp), intent(in) :: rref, p, pref, n, dt
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

end module

submodule(burn_state_interface) burn_state_implementation
  implicit none
contains

  module procedure burnrate
    this%r_=rref*(p/pref)**n ! calculate burn rate
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

end submodule

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
    procedure :: calmdotgen
    procedure :: mdotgen
    procedure :: edotgen
  end type

  interface

    module subroutine calmdotgen(this, rhos, r, surf, cp, Tflame)
      use global_variables, only : dp
      implicit none
      class(generation_rate_t), intent(out) :: this
      real(dp), intent(in) :: rhos, r, surf, cp, Tflame
    end subroutine

    pure module function mdotgen(this)
      use global_variables, only : dp
      implicit none
      class(generation_rate_t), intent(in) :: this
      real(dp) mdotgen
    end function

    pure module function edotgen(this)
      use global_variables, only : dp
      implicit none
      class(generation_rate_t), intent(in) :: this
      real(dp) edotgen
    end function

  end interface

end module

submodule(generation_rate_interface) generation_rate_implementation
  implicit none
contains

  module procedure calmdotgen
    this%mdotgen_ = rhos*r*surf
    this%edotgen_ = this%mdotgen_*cp*Tflame
  end procedure

  module procedure edotgen
    edotgen = this%edotgen_
  end procedure

  module procedure mdotgen
    mdotgen = this%mdotgen_
  end procedure

end submodule

module refurbished_rocket_module
  implicit none

contains

subroutine massflow
   USE global_variables
   implicit none
   REAL (8)::mdtx,engyx
   REAL (8)::tx,gx,rx,px,cpx,pcrit,facx,term1,term2,pratio,cstar,ax,hx
   REAL (8):: p1,p2

   mdotos=0.
   edotos=0.  ! initially set them to zero prior to running this loop

     p1=p
     p2=pamb
     ax=area
     IF(p1.GT.p2) THEN
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
    IF((1./pratio).LT.pcrit) then
        ! choked flow
        cstar=sqrt((1./gx)*((gx+1.)/2.)**((gx+1.)/(gx-1.))*rx*tx)
        mdtx=px*ax/cstar
    else
        ! unchoked flow
      facx=pratio**((gx-1.)/gx)
      term1=SQRT(gx*rx*tx/facx)
      term2=SQRT((facx-1.)/(gx-1.))
      mdtx=SQRT(2.)*px/pratio/rx/tx*facx*term1*term2*ax
    end if
    engyx=mdtx*hx  ! reformulate based on enthalpy of the chamber
    mdotos=mdtx*dsigng ! exiting mass flow (could be negative "dsigng")
    edotos=engyx*dsigng ! exiting enthalpy
end subroutine

subroutine addmass(mdotgen, edotgen, dt)
    use global_variables, only : dp, mdotos, edotos, mcham, echam
    implicit none
    real(dp), intent(in) :: mdotgen, edotgen, dt
    mcham=mcham+(mdotgen-mdotos)*dt
    echam=echam+(edotgen-edotos)*dt
end subroutine

subroutine calct
    use global_variables, only : mcham, echam, cv, t
    implicit none
    t=echam/mcham/cv
end subroutine

subroutine calcp(vol)
    use global_variables, only : dp, p, mcham, rgas, t
    implicit none
    real(dp), intent(in) :: vol
    p=mcham*rgas*t/vol
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
use global_variables
implicit none

character(len=*), intent(in) :: input_file
type(results_t) refurbished_rocket

character(len=max_errmsg_len) error_message
integer io_status, file_unit
integer, parameter :: success = 0

type(burn_state_t) burn_state
type(geometry_t) geometry
type(generation_rate_t) generation_rate

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
cp = c_p_
mw = MW_

read(file_unit, nml=state_list)
t = temperature_
p = pressure_

read(file_unit, nml=combustion_list)
Tflame = T_flame_
rref   = r_ref_
n      = n_

read(file_unit, nml=grain_list)
call geometry%define(vol = 1._dp, id = id_, od = od_, length = length_)
rhos   = rho_solid_

read(file_unit, nml=nozzle_list)
dia = dia_
cf  = C_f_

close(file_unit)

call burn_state%set_db(0._dp)   ! initialize propellant burn distance

psipa=6894.76d0   ! unit conversion factor: pascals per psi
pref=3000d0*psipa ! constant reference pressure for burn-rate calculation

nsteps=nint(tmax/dt) ! number of time steps

allocate(output(0:nsteps,6)) ! preallocate an output array


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

!! now begin calculating and initializing
! gas variables
  rgas=ru/mw
  cv=cp-rgas
  g=cp/cv

  area=pi/4d0*dia**2.0d0 ! nozzle area

  pamb=101325d0 ! atmospheric pressure

!  calculate initial mass and energy in the chamber
  associate(V=>geometry%vol())
    mcham=p*V/rgas/t  ! use ideal gas law to determine mass in chamber
    output(0,:)=[time,p,t,mdotos,thrust,V]
  end associate
  echam=mcham*cv*t ! initial internal energy in chamber


  do i=1,nsteps
   call burn_state%burnrate(rref, p, pref, n, dt)
   call geometry%calcsurf(burn_state, dt)
   call generation_rate%calmdotgen(rhos, burn_state%r(), geometry%surf(burn_state%db()), cp, Tflame)  ! [mdot,engy,dsign]= massflow(p1,pamb,t1,tamb,cp,cp,rgas,rgas,g,g,area)
   call massflow
   call addmass(generation_rate%mdotgen(), generation_rate%edotgen(), dt)
   call calct
   associate(vol=>geometry%vol())
     call calcp(vol)
     call calcthrust
     time=time+dt
     output(i,:)=[time,p,t,mdotos,thrust,vol]
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
