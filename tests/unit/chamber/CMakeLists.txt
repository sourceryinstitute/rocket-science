file( COPY volfil.inp DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable( test-chamber
  test-chamber.f90
)
target_link_libraries( test-chamber
  PRIVATE utilities
)
target_link_libraries( test-chamber
  PRIVATE modules
)
