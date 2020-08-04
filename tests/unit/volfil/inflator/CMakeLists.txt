file( COPY volfil.inp DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable( test-inflator
  test-inflator.f90
)
target_link_libraries( test-inflator
  PRIVATE utilities
)
target_link_libraries( test-inflator
  PRIVATE modules
)
