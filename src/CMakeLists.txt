add_subdirectory(modules)
add_subdirectory(utilities)

add_executable(volfil
  main.f90
)
target_link_libraries( volfil
  PRIVATE modules
)
target_link_libraries( volfil
  PRIVATE utilities
)
