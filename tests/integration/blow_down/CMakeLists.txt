foreach( input_file
  volfil.inp
)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/${input_file}"
    "${CMAKE_CURRENT_BINARY_DIR}/${input_file}"
    COPYONLY
  )
endforeach()

if(${benchmark})
  # volfil will output
else()
  set(output_file "volfil.out")
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/${output_file}"
    "${CMAKE_CURRENT_BINARY_DIR}/${output_file}"
    COPYONLY
  )
endif()
