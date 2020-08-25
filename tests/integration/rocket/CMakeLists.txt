foreach( input_file
  rocket.inp
  gnuplot.inp
)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/${input_file}"
    "${CMAKE_CURRENT_BINARY_DIR}/${input_file}"
    COPYONLY
  )
endforeach()
