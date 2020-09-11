submodule(flow_rate_interface) flow_rate_implementation
  !! Encapsulate mass and energy flow rates to facilitate procedureal programming:
  !! because the energy flow rate depends on the mass flow rate, calculating both
  !! in one procedure avoids redundancy, functions can have only one result so a
  !! flow_rate_t object captures both results in one object.
  implicit none

contains

  module procedure new_flow_rate
    new_flow_rate%m_dot_ = mass_flow_rate
    new_flow_rate%e_dot_ = energy_flow_rate
  end procedure

  module procedure m_dot
    m_dot = this%m_dot_
  end procedure

  module procedure e_dot
    e_dot = this%e_dot_
  end procedure

end submodule flow_rate_implementation
