file( COPY volfil.inp DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable( test-gas
  test-gas.f90
)
target_link_libraries( test-gas
  PRIVATE utilities
)
target_link_libraries( test-gas
  PRIVATE modules
)
