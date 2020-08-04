file( COPY volfil.inp DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable( test-numerics
  test-numerics.f90
)
target_link_libraries( test-numerics
  PRIVATE utilities
)
target_link_libraries( test-numerics
  PRIVATE modules
)
