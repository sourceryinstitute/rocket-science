add_subdirectory(utilities)
add_subdirectory(modules)
add_subdirectory(reference)

add_executable(rocket_science
  main.f90
)
target_link_libraries(rocket_science
  PRIVATE modules
  PRIVATE reference
  PRIVATE utilities
)
