add_library(refurbished
  burn_state_t_implementation.f90
  burn_state_t_interface.f90
  chamber_gas_t_implementation.f90
  chamber_gas_t_interface.f90
  constants.f90
  flow_rate_t_implementation.f90
  flow_rate_t_interface.f90
  nozzle_t_interface.f90
  nozzle_t_implementation.f90
  generation_rate_t_implementation.f90
  generation_rate_t_interface.f90
  geometry_t_implementation.f90
  geometry_t_interface.f90
  refurbished_rocket.F90
)
target_link_libraries(refurbished
  PRIVATE utilities
  PRIVATE rocket # required for refurbished_rocket's dependence on results_t
)
