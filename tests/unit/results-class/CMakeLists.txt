configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/rocket.inp"
  "${CMAKE_CURRENT_BINARY_DIR}/rocket.inp"
  COPYONLY
)
add_executable( test-results-class
  test-results-class.f90
)
target_link_libraries( test-results-class
  PRIVATE utilities
  PRIVATE reference
)
