submodule(nozzle_t_interface) nozzle_implementation
  !! Define the nozzle_t user-defined structure constructrors and type-bound procedures
  implicit none
contains

  module procedure new_nozzle_t
    use constants, only : pi
    new_nozzle_t%area_ = pi/4d0*dia**2 ! nozzle area
    new_nozzle_t%C_f_ = C_f
  end procedure

  module procedure calcthrust
    use constants, only : p_amb
    calcthrust = (p-p_amb)*this%area_*this%C_f_ ! correction to thrust (actual vs vacuum thrust)
  end procedure

  module procedure area
    area = this%area_
  end procedure

end submodule nozzle_implementation
