program rocket

use mod1
implicit none
integer i
! this is a basic program of a single stage
! rocket motor flowing out of a nozzle, assuming
! a thrust coefficient and ignoring the complexities of
! what happens to thrust at low pressures, i.e. shock in the nozzle


! define initial variables and constants
! gas variables
  cp=1500d0 ! j/kg/K
  mw=28d0   ! kg/mol

! define geometry
  vol= 1.0d0 ! cubic meters
  dia=0.1d0 ! meters
  cf=1.7d0 ! nozzle thrust coefficient

! define propellent grain geom; simple outward burning cylinder.
! outer diameter is inhibited since this is a cast propellent meaning
! it was poured into the tube/chamber and only the inner diameter
! burns when ignited 
  id=.25d0  ! inner diameter of propellant
  od=0.5d0  ! outder diameter
  length=1.0d0 ! propellant grain length
  rref=.05d0  ! propellant burn rate at Pref  (m/s)
  rhos=2000d0 ! kg/m3
  psipa=6894.76d0 ! pascals per psi (constant)
  pref=3000d0*psipa ! reference pressure (constant)
  db=0d0 ! initial burn distance

! calculate time related values
  dt=0.001d0
  tmax=12.0d0 ! time to stop calculating performance
  nsteps=nint(tmax/dt) ! number of time steps

! preallocate an output file for simulation infomration
  allocate(output(nsteps,4))
  output=0d0 ! initialize to zero

  thrust=0d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

! now begin calculating and initializing
! gas variables
  rgas=8314d0/mw
  cv=cp-rgas
  g=cp/cv
  pi=3.14159d0
  area=pi/4d0*dia**2.0d0

  t=300d0 ! initial temp (Kelvin)
  pamb=101325d0 ! atmospheric pressure
  p=pamb  ! initial chamber pressure

!  calculate initial mass and energy in the chamber
  mcham=p*vol/rgas/t
  echam=mcham*cv*t
  time=0d0

  do i=1,nsteps
  
   call calmdotgen
   call massflow
  ! [mdot,engy,dsign]= massflow(p1,pamb,t1,tamb,cp,cp,rgas,rgas,g,g,area)
   call addmass
   call calct
   call calcp
   call calcthrust
   output(i,:)=[time,p,t,thrust]
   time=time+dt
  enddo 

  end program


  subroutine calmdotgen
  use mod1
  r=rref*(p/pref)**n ! calculate burn rate

  surf=pi*(id+2d0*r)*length
  if(id+2d0*db.gt.od) surf=0d0  ! we hit the wall and burned out
  if(i==1) surf=pi*id*length! no burn distance

  mdotgen=rhos*r*surf

  end subroutine




  subroutine massflow
   USE mod1
   implicit none
   REAL (8)::mdtx,engyx
   INTEGER::i,j,k
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
   
                   mdotos=mdtx*dsigng
                   edotos=engyx*dsigng
   
                   texit=tx
   
   
         end subroutine

  subroutine addmass 
    use mod1
    mcham=mcham+mdotgen*dt-mdotos*dt*dsigng
    echam=echam+edotgen*dt-edotos*dt*dsigng
  end subroutine

  subroutine calct
     use mod1
    t=echam/mcham/cv
  end subroutine

  subroutine calcp
    use mod1
    p=mcham*rgas*t/vol
  end subroutine

  subroutine calcthrust
    thrust=mdotos*area*cf
  end subroutine
