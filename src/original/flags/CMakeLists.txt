foreach( input_file
  volfil.inp
)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/${input_file}"
    "${CMAKE_CURRENT_BINARY_DIR}/${input_file}"
    COPYONLY
  )
endforeach()

add_executable( test-flags
  test-flags.f90
)
target_link_libraries( test-flags
  PRIVATE utilities
)
target_link_libraries( test-flags
  PRIVATE modules
)
