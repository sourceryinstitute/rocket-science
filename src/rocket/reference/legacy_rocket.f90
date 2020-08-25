
module mod1
implicit none
save
integer, parameter :: precision=15, range=307
integer, parameter :: dp = selected_real_kind(precision, range)
real(dp), parameter :: pi=3.1415926539
real(dp), parameter :: RU=8314d0

real(dp):: cp,cv,g,rgas,mw,vol,dia,cf,id,od,length,rref,rhos,psipa,pref
real(dp):: db,dt,tmax,Tflame
real(dp):: thrust, area, r, n, surf,mdotgen,mdotout,edotgen,edotout,energy
real(dp):: mdotos, edotos, texit, dsigng,pamb,p,t
real(dp):: mcham,echam,time
integer nsteps,i
real(dp), allocatable :: output(:,:)

end module

subroutine burnrate
  use mod1
  implicit none
  r=rref*(p/pref)**n ! calculate burn rate
  db=db+r*dt  ! calculate incremental burn distance
  !print * , 'i,r',i,r
end subroutine

subroutine calcsurf
  ! cylinder burning from id outward and from both ends along the length
  use mod1
  implicit none

  if(i==1) db=0d0 ! at first time step, don't regress the grain
  surf=pi*(id+2.0d0*db)*(length-2.0d0*db)+0.5d0*pi*(od**2.0d0-(id+2.0*db)**2.0d0)

  if(id+2d0*db.gt.od.or.db.gt.length/2d0) THEN
     surf=0d0  ! we hit the wall and burned out
     r=0  ! turn off burn rate so burn distance stops increasing
   endif

   if (i>1) vol=vol+r*surf*dt ! increment the interior volume of the chamber a little
  ! print *,'surf',surf
end subroutine

subroutine calmdotgen
  use mod1
  implicit none
  mdotgen=rhos*r*surf
  edotgen=mdotgen*cp*Tflame
  !print *,'mgen,egen',mdotgen,edotgen
end subroutine

subroutine massflow
   USE mod1
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

subroutine addmass
    use mod1
    implicit none
    mcham=mcham+(mdotgen-mdotos)*dt
    echam=echam+(edotgen-edotos)*dt
end subroutine

subroutine calct
    use mod1
    implicit none
    t=echam/mcham/cv
end subroutine

subroutine calcp
    use mod1
    implicit none
    p=mcham*rgas*t/vol
  !  print *,'pt',time,p,t
end subroutine

subroutine calcthrust
    use mod1
    implicit none
    thrust=(p-pamb)*area*cf ! correction to thrust (actual vs vacuum thrust)
end subroutine

!!  Main program


function legacy_rocket(input_file)
  !! this is a basic program of a single stage
  !! rocket motor flowing out of a nozzle, assuming
  !! a thrust coefficient and ignoring the complexities of
  !! what happens to thrust at low pressures, i.e. shock in the nozzle

use assertions_interface, only : assert, max_errmsg_len
use results_interface, only : results_t
use mod1
implicit none

character(len=*), intent(in) :: input_file
type(results_t) legacy_rocket

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
  vol= 1.0d0 ! cubic meters

!  propellent grain is a cylinder burning radially outward and axially inward.
! outer diameter is inhibited because this is a cast propellent: it was poured
! into the tube/chamber and only the inner diameter burns when ignited.

  ! propellant burn rate information
  psipa=6894.76d0 ! pascals per psi (constant)
  pref=3000d0*psipa ! reference pressure (constant)
  db=0d0 ! initial burn distance

  nsteps=nint(tmax/dt) ! number of time steps

! preallocate an output file for simulation infomration
  allocate(output(nsteps,5))
  output=0d0 ! initialize to zero

  thrust=0d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

!! now begin calculating and initializing
! gas variables
  rgas=ru/mw
  cv=cp-rgas
  g=cp/cv

  area=pi/4d0*dia**2.0d0 ! nozzle area

  pamb=101325d0 ! atmospheric pressure

!  calculate initial mass and energy in the chamber
  mcham=p*vol/rgas/t  ! use ideal gas law to determine mass in chamber
  echam=mcham*cv*t ! initial internal energy in chamber

  time=0d0

  do i=1,nsteps
   call burnrate
   call calcsurf

   call calmdotgen
   call massflow
  ! [mdot,engy,dsign]= massflow(p1,pamb,t1,tamb,cp,cp,rgas,rgas,g,g,area)
   call addmass
   call calct
   call calcp
   call calcthrust
   output(i,:)=[time,p,t,mdotos,thrust]
   time=time+dt

  enddo

  legacy_rocket = results_t(header="time   pressure   temperature   mdotos  thrust", body=output)

end function legacy_rocket
