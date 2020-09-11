add_subdirectory(utilities)
add_subdirectory(rocket)
add_subdirectory(legacy)

add_executable(rocket_science
  main.f90
)
target_link_libraries(rocket_science
  PRIVATE rocket
  PRIVATE legacy
  PRIVATE utilities
)
