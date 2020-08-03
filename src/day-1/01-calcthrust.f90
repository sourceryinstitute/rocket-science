  subroutine calcthrust(p,area,cf, thrust)
    implicit none
    real p, area, cf, thrust
    thrust = p*area*cf
  end subroutine
