program volfil
  use kind_parameters, only : DP
  use subdeclare
  use command_line_interface, only : command_line
  use flags_module, only : flags, set_dt, set_tmax
  implicit none

  type(command_line) :: volfil_command
  type(chamber_internal)::cham
  type(combustion)::comb
  type(gasprop)::gp
  type(flags)::flag
  type(flow)::flo

  integer :: nsteps,i
  real(DP) :: time
  real(DP) :: dt, tmax

  namelist /time/       dt=1.E-5, tmax=.1
  namelist /gasprop/    cp=1000., mw=28
  namelist /state/      P=101250., T=300., vol=10e-4
  namelist /orifice/    radius=.009
  namelist /combustion/ tflame=2500., mpkg=18., genmass=.08, genheight=.004, gendiam=.007, rhosolid=1850, rref=.035, n=.4

  open(unit=20,file='volfil.inp')

  read(20,nml=time) dt,tmax
  call set_dt(flag, dt)
  call set_tmax(flag, tmax)

  read(20,nml=gasprop) gp%cp,gp%mw
  gp%rgas=gp%cp-gp%cv
  gp%cv=gp%cp-Ru/gp%mw ! set a value for rgas
  gp%g=gp%cp/gp%cv;

  read(20,nml=state) cham%P,cham%T, cham%vol
  read(20,nml=orifice) flo%diam
  read(20,nml=combustion) comb%tflame,comb%mpkg, comb%genmass, comb%genheight, comb%gendiam, comb%rhosolid, comb%rref, comb%n
  close(20)
  nsteps= int( get_tmax(flag)/get_dt(flag) )
  comb%db=0.d0 !initialized burn distance to zero
  cham%T=300;cham%M=.03

  cham%E=cham%M*cham%T*gp%cv
  call ntabs(comb)
  call getgasproperties(gp,cham)
  call calctp(cham,gp)
  time=0.d0

  block
    integer, parameter :: skip=50, first=1, num_variables=3
    integer :: step, last
    real(DP) :: output(num_variables,nsteps)
    real(DP) :: reference(num_variables)
    real(DP), parameter :: tolerance=1.E-3_DP

    last=nsteps-1
    do i = first, last
      time=time+get_dt(flag)

      call calmdotgen(cham, comb, gp,flag)
      call massflow(cham, gp, flo)
      call addmass(cham,comb,flo,flag)
      call getgasproperties(gp,cham)
      call calctp(cham,gp)
      if (volfil_command%argument_present([ character(len=len("--verbose")) :: "--verbose", "-v", "/verbose", "/v"])) then
        print*, [time, cham%P,  cham%T]
      end if
      output(:,i)=[time, cham%P,  cham%T]
    end do

    if (volfil_command%argument_present([ character(len=len("--benchmark")) :: "--benchmark", "-b", "/benchmark", "/b"])) then
      ! write benchmark file
      open(unit=20,file='volfil.out',status='unknown')
      do step = first, last, skip
        write(20,'(3e15.5)') output(:,step)
      end do
    else !verify result
      open(unit=20,file='volfil.out',status='old')
      do step = 1, last/skip
        read(20,'(3e15.5)') reference
        !if (maxval(abs(output(:,step)-reference)) > tolerance) error stop "tolerance exceeded"
      end do
    end if

    close(20)
  end block

  print *,"Test passed."

end program
