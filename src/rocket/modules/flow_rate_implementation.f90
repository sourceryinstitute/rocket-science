submodule(flow_rate_interface) flow_rate_implementation
  !! Encapsulate mass and energy flow rates to facilitate procedureal programming:
  !! because the energy flow rate depends on the mass flow rate, calculating both
  !! in one procedure avoids redundancy, functions can have only one result so a
  !! flow_rate_t object captures both results in one object.
  implicit none

contains

  module procedure new_flow_rate
    new_flow_rate%m_dot_out_ = mass_outflow_rate
    new_flow_rate%e_dot_out_ = energy_outflow_rate
  end procedure

  module procedure m_dot_out
    m_dot_out = this%m_dot_out_
  end procedure

  module procedure e_dot_out
    e_dot_out = this%e_dot_out_
  end procedure

end submodule flow_rate_implementation
