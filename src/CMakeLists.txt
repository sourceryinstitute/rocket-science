add_subdirectory(utilities)
add_subdirectory(rocket)
add_subdirectory(legacy)
add_subdirectory(refurbished)

add_executable(rocket-science
  main.f90
)
target_link_libraries(rocket-science
  PRIVATE rocket
  PRIVATE legacy
  PRIVATE refurbished
  PRIVATE utilities
)
