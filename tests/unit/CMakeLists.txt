foreach( dir
  chamber
  gas
  hole
  inflator
  numerics
  combustion
)
  add_subdirectory(${dir})
endforeach()
