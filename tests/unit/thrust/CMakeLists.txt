add_executable( test-thrust
  test-thrust.f90
)
target_link_libraries( test-thrust
  PRIVATE utilities
  PRIVATE reference
)
