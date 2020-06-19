foreach( dir
  chamber
  gas
  hole
  inflator
  numerics
  pyro
)
  add_subdirectory(${dir})
endforeach()
