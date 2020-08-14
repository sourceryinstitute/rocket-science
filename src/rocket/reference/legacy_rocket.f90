
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
   REAL (8):: p1,p2,mindt

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


function legacy_rocket() result(capture_output)

use mod1
implicit none

real(dp), allocatable :: capture_output(:,:)

! this is a basic program of a single stage
! rocket motor flowing out of a nozzle, assuming
! a thrust coefficient and ignoring the complexities of
! what happens to thrust at low pressures, i.e. shock in the nozzle

!! modifications as of 8/6/20:
!  going to make more/simpler subroutines to avoid confusion
!  in an attempt to avoid confusion with variable names


! define initial variables and constants
! gas variables
  cp=1500d0 ! j/kg/K
  mw=28d0   ! kg/mol

! define geometry
  vol= 1.0d0 ! cubic meters
  dia=0.1d0 ! nozzle diameter (meters)
  cf=1.7d0 ! nozzle thrust coefficient

! define propellent grain geom; simple outward burning cylinder.
! outer diameter is inhibited since this is a cast propellent meaning
! it was poured into the tube/chamber and only the inner diameter
! burns when ignited
!! geometry of the propellant
!! I am modifying the burn so that it not only burns radially, but also from the front
!! and back ends.  e.g adding a few  hollow cylinders burning
  id=.25d0  ! inner diameter of propellant
  od=0.5d0  ! outder diameter
  length=1.0d0 ! propellant grain length

  !! propellant burn rate information
  rref=.05d0  ! propellant burn rate at Pref  (m/s)
  rhos=2000d0 ! kg/m3
  psipa=6894.76d0 ! pascals per psi (constant)
  pref=3000d0*psipa ! reference pressure (constant)
  n=0.4 ! burn rate exponent
  db=0d0 ! initial burn distance
  Tflame=4000 ! temperature in Kelvin

!! calculate time related values
  dt=0.005d0
  tmax=5.0d0 ! time to stop calculating performance
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

  t=300d0 ! initial temp (Kelvin)
  pamb=101325d0 ! atmospheric pressure
  p=pamb  ! initial chamber pressure

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

  capture_output = output

end function legacy_rocket
