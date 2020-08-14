configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/rocket.inp"
  "${CMAKE_CURRENT_BINARY_DIR}/rocket.inp"
  COPYONLY
)
add_executable( test-reference-data-output
  test-reference-data-output.f90
)
target_link_libraries( test-reference-data-output
  PRIVATE utilities
  PRIVATE reference
)
