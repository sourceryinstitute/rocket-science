configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/rocket.inp"
  "${CMAKE_CURRENT_BINARY_DIR}/rocket.inp"
  COPYONLY
)
add_executable( test-nozzle-class
  test-nozzle-class.f90
)
target_link_libraries( test-nozzle-class
  PRIVATE utilities
  PRIVATE rocket
)
