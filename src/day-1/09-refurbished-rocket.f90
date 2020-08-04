function refurbished_rocket() result(output)

use mod1

implicit none
real, allocatable :: output(:,:)
integer  i
! this is a basic program of a single stage
! rocket motor flowing out of a nozzle, assuming
! a thrust coefficient and ignoring the complexities of
! what happens to thrust at low pressures, i.e. shock in the nozzle


! define initial variables and constants
! gas variables
  cp=1500d0 ! j/kg/K
  mw=28d0   ! kg/mol
  tflame=4000.d0 ! Kelvin

! define geometry
  vol= 1.0d0 ! cubic meters
  dia=0.05d0 ! meters
  cf=1.7d0 ! nozzle thrust coefficient

! define propellent grain geom; simple outward burning cylinder.
! outer diameter is inhibited since this is a cast propellent meaning
! it was poured into the tube/chamber and only the inner diameter
! burns when ignited
  id=0.25d0  ! inner diameter of propellant
  od=0.5d0  ! outder diameter
  length=1.0d0 ! propellant grain length
  rref=0.05d0  ! propellant burn rate at Pref  (m/s)
  rhos=2000d0 ! kg/m3
  n=.4 ! burn rate exponent
  psipa=6894.76d0 ! pascals per psi (constant)
  pref=3000d0*psipa ! reference pressure (constant)
  db=0d0 ! initial burn distance

! calculate time related values
  dt=0.001d0
  tmax=15.0d0 ! time to stop calculating performance
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
  area=pi/4.0d0*dia**2.0d0

  t=300d0 ! initial temp (Kelvin)
  pamb=101325d0 ! atmospheric pressure
  p=pamb  ! initial chamber pressure

!  calculate initial mass and energy in the chamber
  mcham=p*vol/rgas/t
  echam=mcham*cv*t
  time=0d0
  do i=1,nsteps

   call calc_m_dot_gen
   call mass_flow
  ! [mdot,engy,dsign]= massflow(p1,pamb,t1,tamb,cp,cp,rgas,rgas,g,g,area)
   call add_mass
   call calc_t
   call calc_p
   call calc_thrust
   output(i,:)=[time,p,t, thrust]
   time=time+dt
  enddo

  do i=1,nsteps-1
    print *, output(i,:)
  enddo
end function


  subroutine calc_m_dot_gen
  use mod1
  r=rref*(p/pref)**n ! calculate burn rate

  surf=pi*(id+2d0*db)*length
  if((id/2.0d0+db).gt.od/2.0d0) then
      surf=0.0d0 ! burned out to wall
      r=0.0d0 ! stop adding to burn distance because surface area becomes negative
  endif
  if(i==1) surf=pi*id*length! no burn distance

  mdotgen=rhos*r*surf
  edotgen=mdotgen*cp*tflame
  db=db+r*dt ! increment burn distance
  vol=vol+r*surf*dt ! increment the volume due to burn back
  end subroutine




  subroutine mass_flow
   USE mod1
   implicit none
   REAL (8)::mdtx,engyx
   INTEGER::i
   REAL (8)::tx,gx,rx,px,cpx,pcrit,facx,term1,term2,pratio,cstar,ax,hx
   REAL (8):: p1,p2,mindt

   mdotos=0.
   edotos=0.  ! initially set them to zero prior to running this loop

            p1=p
            p2=pamb
            ax=area
              IF(p1.GT.p2) THEN
                          dsigng=1.0d0
                          tx=t
                          gx=g
                          rx=rgas
                          px=p
                          cpx=cp
                          hx=cp*t
                          pratio=p1/p2
                  else
                          dsigng=-1.0d0
                          tx=300d0
                          gx=g
                          rx=rgas
                          px=pamb
                          cpx=cp
                          hx=cp*300d0
                          pratio=p2/p1
                  end if

                  pcrit=(2.d0/(gx+1.d0))**(gx/(gx-1.d0))
                  IF((1.d0/pratio).LT.pcrit) then
                          ! choked flow
                          cstar=sqrt((1.d0/gx)*((gx+1.d0)/2.d0)**((gx+1.d0)/(gx-1.d0))*rx*tx)
                          mdtx=px*ax/cstar
                  else
                          ! unchoked flow
                          facx=pratio**((gx-1.d0)/gx)
                          term1=SQRT(gx*rx*tx/facx)
                          term2=SQRT((facx-1.d0)/(gx-1.d0))
                          mdtx=SQRT(2.d0)*px/pratio/rx/tx*facx*term1*term2*ax
                  end if


                   engyx=mdtx*hx  ! reformulate based on enthalpy of the chamber

                   mdotos=mdtx*dsigng
                   edotos=engyx*dsigng

                   texit=tx


         end subroutine

  subroutine add_mass
    use mod1
    implicit none
    mcham=mcham+mdotgen*dt-mdotos*dt
    echam=echam+edotgen*dt-edotos*dt
  end subroutine

  subroutine calc_t
     use mod1
     implicit none
    t=echam/mcham/cv
  end subroutine

  subroutine calc_p
    use mod1
    implicit none
    p=mcham*rgas*t/vol
  end subroutine

  subroutine calc_thrust
    use mod1
    implicit none
    thrust=p*area*cf
  end subroutine
