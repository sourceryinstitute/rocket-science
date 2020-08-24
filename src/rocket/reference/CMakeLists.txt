add_library(reference
  legacy_rocket.f90
)
target_link_libraries(reference
  PRIVATE utilities
  PRIVATE modules
)
