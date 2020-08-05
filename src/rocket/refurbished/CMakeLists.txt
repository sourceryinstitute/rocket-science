add_executable(refurbished_rocket
  module_variables.f90
  refurbished-rocket.f90
)
target_link_libraries(refurbished_rocket
  PRIVATE reference
  PRIVATE utilities
)
