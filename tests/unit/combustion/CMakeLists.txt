file( COPY volfil.inp DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable( test-combustion
  test-combustion.f90
)
target_link_libraries( test-combustion
  PRIVATE utilities
)
target_link_libraries( test-combustion
  PRIVATE modules
)
