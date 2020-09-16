module global_variables
implicit none
save
integer, parameter :: precision=15, range=307
integer, parameter :: dp = selected_real_kind(precision, range)
real(dp), parameter :: pi=3.1415926539
real(dp), parameter :: RU=8314d0

real(dp):: cp,cv,g,rgas,mw,dia,cf,id,od,length,rref,rhos,psipa,pref
real(dp):: dt,tmax,Tflame
real(dp):: thrust, area, n, mdotgen,mdotout,edotgen,edotout,energy
real(dp):: mdotos, edotos, texit, pamb,p,t
real(dp):: mcham,echam,time
integer nsteps,i
real(dp), allocatable :: output(:,:)

end module

module burn_state_interface
  use global_variables, only : dp
  implicit none

  private

  type, public :: burn_state_t
    private
    real(dp):: r_, db_
  contains
    procedure :: set_db
    procedure :: set_r
    procedure :: r
    procedure :: db
    procedure :: calculate_burn_rate
  end type

  interface

    module subroutine calculate_burn_rate(this, rref, p, pref, n, dt)
      implicit none
      class(burn_state_t), intent(inout) :: this
      real(dp), intent(in) :: rref, p, pref, n, dt
    end subroutine

    module subroutine set_db(this, db)
      implicit none
      class(burn_state_t), intent(inout) :: this
      real(dp), intent(in) :: db
    end subroutine

    module subroutine set_r(this, r)
      implicit none
      class(burn_state_t), intent(inout) :: this
      real(dp), intent(in) :: r
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

end module

submodule(burn_state_interface) burn_state_implementation
  implicit none
contains

  module procedure calculate_burn_rate
    this%r_ = rref*(p/pref)**n ! calculate burn rate
    this%db_ = this%db_+this%r_*dt  ! calculate incremental burn distance
  end procedure

  module procedure set_r
    this%r_ = r
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

end submodule

module geometry_interface
  use global_variables, only : dp
  implicit none

  private

  type, public :: geometry_t
    private
    real(dp):: surf_, vol_
  contains
    procedure :: set_vol
    procedure :: set_surf
    procedure :: surf
    procedure :: vol
    procedure :: calculate_surface_area
  end type

  interface

    module subroutine calculate_surface_area(this, burn_state, length, id, od, dt)
      ! cylinder burning from id outward and from both ends along the length
      use global_variables, only : dp
      use burn_state_interface, only : burn_state_t
      implicit none
      class(geometry_t), intent(inout) :: this
      type(burn_state_t), intent(inout) :: burn_state
      real(dp), intent(in) :: length, id, od, dt
    end subroutine

    module subroutine set_vol(this, vol)
      implicit none
      class(geometry_t), intent(inout) :: this
      real(dp), intent(in) :: vol
    end subroutine

    module subroutine set_surf(this, surf)
      implicit none
      class(geometry_t), intent(inout) :: this
      real(dp), intent(in) :: surf
    end subroutine

    pure module function vol(this)
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp) vol
    end function

    pure module function surf(this)
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp) surf
    end function

  end interface

end module

submodule(geometry_interface) geometry_implementation
  implicit none
contains

  module procedure calculate_surface_area
    use global_variables, only : dp, pi

    associate(db => burn_state%db())
      this%surf_=pi*(id+2.0d0*db)*(length-2.0d0*db)+0.5d0*pi*(od**2.0d0-(id+2.0*db)**2.0d0)

      if(id+2d0*db.gt.od.or.db.gt.length/2d0) THEN
         this%surf_=0d0  ! we hit the wall and burned out
         call burn_state%set_r(0._dp)  ! turn off burn rate so burn distance stops increasing
       endif

      associate(r => burn_state%r())
        this%vol_=this%vol_+r*this%surf_*dt ! increment the interior volume of the chamber a little
      end associate
    end associate

  end procedure

  module procedure set_surf
    this%surf_ = surf
  end procedure

  module procedure set_vol
    this%vol_ = vol
  end procedure

  module procedure vol
    vol = this%vol_
  end procedure

  module procedure surf
    surf = this%surf_
  end procedure

end submodule

module refurbished_rocket_module
  implicit none

contains

  subroutine calculate_m_dot_generated(burn_state, rhos, surf, cp, Tflame, mdotgen, edotgen)
    use global_variables, only : dp
    use burn_state_interface , only : burn_state_t
    real(dp), intent(in) :: rhos, surf, cp, Tflame
    real(dp), intent(out) :: mdotgen, edotgen
    type(burn_state_t), intent(in) :: burn_state

    associate(r=>burn_state%r())
      mdotgen=rhos*r*surf
      edotgen=mdotgen*cp*Tflame
    end associate
  end subroutine

  subroutine calculate_mass_flow(pamb, area, rgas, g, cp, edotos, mdotos, p, t)
     USE global_variables, only : dp
     real(dp), intent(in) :: pamb, area, rgas, g, cp, p, t
     real(dp), intent(out) :: edotos, mdotos

     REAL (dp)::mdtx,engyx, dsigng
     REAL (dp)::tx,gx,rx,px,cpx,pcrit,facx,term1,term2,pratio,cstar,ax,hx
     REAL (dp):: p1,p2

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

  subroutine add_mass(mdotgen, mdotos, edotgen, edotos, dt, mcham, echam)
      use global_variables, only : dp
      real(dp), intent(in) :: mdotgen, mdotos, edotgen, edotos, dt
      real(dp), intent(inout) :: mcham, echam
      mcham=mcham+(mdotgen-mdotos)*dt
      echam=echam+(edotgen-edotos)*dt
  end subroutine

  subroutine calculate_temperature(echam, mcham, cv, t)
     use global_variables, only : dp
     real(dp), intent(in) :: echam, mcham, cv
     real(dp), intent(out) :: t
     t=echam/mcham/cv
  end subroutine

  subroutine calculcate_pressure(mcham, rgas, t, vol, p)
      use global_variables, only : dp
      real(dp), intent(in) :: mcham, rgas, t, vol
      real(dp), intent(out) :: p
      p=mcham*rgas*t/vol
  end subroutine

  subroutine calculate_thrust(p, pamb, area, cf, thrust)
      use global_variables, only : dp
      real(dp), intent(in) :: p, pamb, area, cf
      real(dp), intent(out) :: thrust
      thrust=(p-pamb)*area*cf ! correction to thrust (actual vs vacuum thrust)
  end subroutine

  function refurbished_rocket(input_file)
    !! this is a basic program of a single stage
    !! rocket motor flowing out of a nozzle, assuming
    !! a thrust coefficient and ignoring the complexities of
    !! what happens to thrust at low pressures, i.e. shock in the nozzle

  use assertions_interface, only : assert, max_errmsg_len
  use results_interface, only : results_t
  use global_variables, only : &
    dt, tmax, cp, mw, t, p, Tflame, rref, n, id, od, length, rhos, dia, cf, rgas, cv, g, area, pamb, dp, output, echam, &
    mcham, pi, edotos, mdotos, nsteps, pref, psipa, ru, time, i, thrust, t, mdotgen, edotgen
  use burn_state_interface, only : burn_state_t
  use geometry_interface, only : geometry_t

  type(burn_state_t) burn_state
  type(geometry_t) geometry

  character(len=*), intent(in) :: input_file
  type(results_t) refurbished_rocket

  character(len=max_errmsg_len) error_message
  integer io_status, file_unit
  integer, parameter :: success = 0

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
  id     = id_
  od     = od_
  length = length_
  rhos   = rho_solid_

  read(file_unit, nml=nozzle_list)
  dia = dia_
  cf  = C_f_

  close(file_unit)

  ! define geometry
    call geometry%set_vol(1.0d0) ! cubic meters

  !  propellent grain is a cylinder burning radially outward and axially inward.
  ! outer diameter is inhibited because this is a cast propellent: it was poured
  ! into the tube/chamber and only the inner diameter burns when ignited.

    ! propellant burn rate information
    psipa=6894.76d0 ! pascals per psi (constant)
    pref=3000d0*psipa ! reference calculcate_pressure (constant)
    call burn_state%set_db(0d0) ! initial burn distance

    nsteps=nint(tmax/dt) ! number of time steps

  ! preallocate an output file for simulation infomration
    allocate(output(0:nsteps,6))
    output=0d0 ! initialize to zero

    thrust=0d0
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

  !! now begin calculating and initializing
  ! gas variables
    rgas=ru/mw
    cv=cp-rgas
    g=cp/cv

    area=pi/4d0*dia**2.0d0 ! nozzle area

    pamb=101325d0 ! atmospheric calculcate_pressure

  !  calculate initial mass and energy in the chamber
    mcham=p*geometry%vol()/rgas/t  ! use ideal gas law to determine mass in chamber
    echam=mcham*cv*t ! initial internal energy in chamber

    time=0d0

    output(0,:)=[time,p,t,mdotos,thrust,geometry%vol()]

    do i=1,nsteps
     call burn_state%calculate_burn_rate(rref, p, pref, n, dt)
     call geometry%calculate_surface_area(burn_state, length, id, od, dt)
     call calculate_m_dot_generated(burn_state, rhos, geometry%surf(), cp, Tflame, mdotgen, edotgen)
       ! [mdot,engy,dsign]= calculate_mass_flow(p1,pamb,t1,tamb,cp,cp,rgas,rgas,g,g,area)
     call add_mass(mdotgen, mdotos, edotgen, edotos, dt, mcham, echam)
     call calculate_temperature(echam, mcham, cv, t)
     call calculcate_pressure(mcham, rgas, t, geometry%vol(), p)
     call calculate_mass_flow(pamb, area, rgas, g, cp, edotos, mdotos, p, t)
     call calculate_thrust(p, pamb, area, cf, thrust)
     time=time+dt
     output(i,:)=[time,p,t,mdotos,thrust,geometry%vol()]
    enddo

    block
      character(len=*), parameter :: header(*) = [ character(len=len("temperatureRefurbished)")) :: "timeRefurbished", &
        "pressureRefurbished", "temperatureRefurbished", "mdotosRefurbished", "thrustRefurbished", "volumeRefurbished"]
      refurbished_rocket = results_t(header, output)
    end block

  end function refurbished_rocket

end module  refurbished_rocket_module
