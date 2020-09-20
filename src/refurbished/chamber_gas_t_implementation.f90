submodule(chamber_gas_t_interface) chamber_gas_t_implementation
  !! Defined the chamber_gas_t user-defined structure constructors and type-bound procedures
  implicit none

contains

  module procedure new_chamber_gas_t
    new_chamber_gas_t%MW_  = MW
    new_chamber_gas_t%c_p_ = c_p
    new_chamber_gas_t%mcham_ = p*V/(new_chamber_gas_t%R_gas()*T)
    new_chamber_gas_t%echam_  = new_chamber_gas_t%mcham_*new_chamber_gas_t%c_v()*T
  end procedure

  module procedure incremented_chamber_gas_t
    incremented_chamber_gas_t%MW_    = old_chamber_gas_t%MW_
    incremented_chamber_gas_t%c_p_   = old_chamber_gas_t%c_p_
    incremented_chamber_gas_t%mcham_ = old_chamber_gas_t%mcham_ + mass_increment
    incremented_chamber_gas_t%echam_ = old_chamber_gas_t%echam_ + energy_increment
  end procedure

  module procedure R_gas
    real(dp), parameter :: R_universal=8314._dp
    R_gas = R_universal/this%MW_
  end procedure

  module procedure c_p
    c_p = this%c_p_
  end procedure

  module procedure c_v
    c_v = this%c_p() - this%R_gas()
  end procedure

  module procedure T
    T = this%echam_/(this%mcham_*this%c_v())
  end procedure

  module procedure calcp
    calcp = this%mcham_*this%R_gas()*this%T()/V
  end procedure

  module procedure g
    g = this%c_p_/this%c_v()
  end procedure

end submodule chamber_gas_t_implementation
