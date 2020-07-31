add_subdirectory(original)
add_subdirectory(reference)

add_executable(rocket_science
  main.f90
)
target_link_libraries(rocket_science
  PRIVATE reference_rocket
  PRIVATE utilities
)
