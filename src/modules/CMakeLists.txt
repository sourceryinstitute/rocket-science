add_library(modules
  numerics_module.f90
  chamber_module.f90
  gas_module.f90
  hole_module.f90
  inflator_module.f90
  numerics_module.f90
  combustion_module.f90
  universal_constants.f90
  persistent_state_module.f90
  flow_rate_module.f90
)

target_link_libraries(modules
  PRIVATE utilities
)
