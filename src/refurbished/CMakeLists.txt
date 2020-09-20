add_library(refurbished
  burn_state_implementation.f90
  burn_state_interface.f90
  constants.f90
  geometry_implementation.f90
  geometry_interface.f90
  refurbished_rocket.f90
)
target_link_libraries(refurbished
  PRIVATE utilities
  PRIVATE rocket
)
