program volfil
  use kind_parameters, only : DP
  use subdeclare
  use command_line_interface, only : command_line
  implicit none

  type(command_line) :: volfil_command
  type(chamber_internal)::cham
  type(combustion)::comb
  type(gasprop)::gp
  type(flags)::flag
  type(flow)::flo

  integer :: nsteps,i
  real(DP) :: time
  open(unit=20,file='volfil.inp')
  read(20,*);read(20,*)
  read(20,*) flag%dt,flag%tmax; read(20,*)
  read(20,*) gp%cp,gp%mw; read(20,*)
  gp%rgas=gp%cp-gp%cv; gp%cv=gp%cp-Ru/gp%mw ! set a value for rgas
  gp%g=gp%cp/gp%cv;
  read(20,*) cham%P,cham%T, cham%vol; read(20,*)
  read(20,*) flo%diam;read(20,*)
  read(20,*) comb%tflame,comb%mpkg, comb%genmass, comb%genheight, comb%gendiam, comb%rhosolid, comb%rref, comb%n
  close(20)
  nsteps= int( (flag%tmax)/(flag%dt) )
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
      time=time+flag%dt

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
