add_library(legacy
  legacy_rocket.f90
)
target_link_libraries(legacy
  PRIVATE utilities
  PRIVATE rocket
)
