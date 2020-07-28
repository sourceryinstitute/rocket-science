add_subdirectory(original)
add_subdirectory(modules)

add_executable(rocket
  main.f90
)

target_link_libraries(rocket
  PRIVATE rocketscience
)
