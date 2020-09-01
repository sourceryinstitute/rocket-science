add_library(modules
  numerics_interface.f90
  numerics_implementation.f90
  chamber_module.f90
  gas_module.f90
  nozzle_interface.f90
  nozzle_implementation.f90
  motor_interface.f90
  motor_implementation.f90
  combustion_interface.f90
  combustion_implementation.f90
  state_interface.f90
  state_implementation.f90
  flow_rate_interface.f90
  flow_rate_implementation.f90
  generation_rate_interface.f90
  generation_rate_implementation.f90
  state_rate_interface.f90
  state_rate_implementation.f90
  grain_interface.f90
  grain_implementation.f90
  results_interface.f90
  results_implementation.f90
)

target_link_libraries(modules
  PRIVATE utilities
)
