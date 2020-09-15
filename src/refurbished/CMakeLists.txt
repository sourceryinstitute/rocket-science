add_library(refurbished
  refurbished_rocket.f90
)
target_link_libraries(refurbished
  PRIVATE utilities
  PRIVATE rocket
)
