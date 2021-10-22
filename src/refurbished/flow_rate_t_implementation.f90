submodule(flow_rate_t_interface) flow_rate_t_implementation
  !! Defined the flow_rate_t user-defined structure constructors and type-bound procedures
  implicit none
contains

  module procedure new_flow_rate_t
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

  end procedure new_flow_rate_t

  module procedure edotos
    edotos = this%edotos_
  end procedure

  module procedure mdotos
    mdotos = this%mdotos_
  end procedure

end submodule flow_rate_t_implementation
