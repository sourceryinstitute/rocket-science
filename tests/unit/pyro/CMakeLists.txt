file( COPY volfil.inp DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable( test-pyro
  test-pyro.f90
)
target_link_libraries( test-pyro
  PRIVATE utilities
)
target_link_libraries( test-pyro
  PRIVATE modules
)
