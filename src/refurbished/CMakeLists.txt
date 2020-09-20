add_library(refurbished
  burn_state_implementation.f90
  burn_state_interface.f90
  constants.f90
  # These must be incorporated by C-preprocessor include statements due to gfortran 11 internal compiler errors
  #flow_rate_implementation.f90
  #flow_rate_interface.f90
  generation_rate_implementation.f90
  generation_rate_interface.f90
  geometry_implementation.f90
  geometry_interface.f90
  refurbished_rocket.F90
)
target_link_libraries(refurbished
  PRIVATE utilities
  PRIVATE rocket
)
