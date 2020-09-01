submodule(generation_rate_interface) generation_rate_implementation
  !! Encapsulate mass and energy generation rates to facilitate functional programming:
  !! because the energy generation rate depends on the mass generation rate, calculating
  !! both in one function avoids redundancy. Functions can have only one result so a
  !! generation_rate_t object captures both results in one object.
  use kind_parameters, only : rkind
  implicit none

contains

  module procedure new_generation_rate
    new_generation_rate%burn_rate_= burn_rate
    new_generation_rate%m_dot_gen_= mass_generation_rate
    new_generation_rate%e_dot_gen_= enthalpy_generation_rate
  end procedure

  module procedure burn_rate
    burn_rate = this%burn_rate_
  end procedure

  module procedure m_dot_gen
    m_dot_gen = this%m_dot_gen_
  end procedure

  module procedure e_dot_gen
    e_dot_gen = this%e_dot_gen_
  end procedure

end submodule generation_rate_implementation
