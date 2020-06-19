file( COPY volfil.inp DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable( test-hole
  test-hole.f90
)
target_link_libraries( test-hole
  PRIVATE utilities
)
target_link_libraries( test-hole
  PRIVATE modules
)
