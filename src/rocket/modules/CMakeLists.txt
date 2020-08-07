add_library(modules
  numerics_module.f90
  chamber_module.f90
  gas_module.f90
  nozzle_module.f90
  motor_module.f90
  numerics_module.f90
  combustion_module.f90
  persistent_state_module.f90
  flow_rate_module.f90
  generation_rate_module.f90
)

target_link_libraries(modules
  PRIVATE utilities
)
