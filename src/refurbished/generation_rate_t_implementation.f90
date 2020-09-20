submodule(generation_rate_t_interface) generation_rate_t_implementation
  !! Defined the generation_rate_t user-defined structure constructors and type-bound procedures
  implicit none
contains

  module procedure calmdotgen
    associate(mdotgen => rhos*r*surf)
      new_generation_rate%mdotgen_ = mdotgen
      new_generation_rate%edotgen_ = mdotgen*cp*Tflame
    end associate
  end procedure

  module procedure edotgen
    edotgen = this%edotgen_
  end procedure

  module procedure mdotgen
    mdotgen = this%mdotgen_
  end procedure

end submodule generation_rate_t_implementation
