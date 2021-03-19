cmake_minimum_required(VERSION 3.16.3)

set (ROCKET_VERSION "1.0.0")
set (ALLOWED_BUILD_TYPES "Debug" "Release")

get_property(multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(multi_config)
  set (CMAKE_CONFIGURATION_TYPES "${ALLOWED_BUILD_TYPES}" CACHE STRING "list of supported configuration types" FORCE)
else()
  set (CMAKE_BUILD_TYPE "Debug" CACHE STRING "Select which configuration to build.")
  set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${ALLOWED_BUILD_TYPES})
endif()

string  (JOIN "\n " GREETING "Version: ${ROCKET_VERSION}\n Build type: ${CMAKE_BUILD_TYPE}\n")
message (STATUS "\n${GREETING}")

project(rocket
  VERSION "${ROCKET_VERSION}"
  LANGUAGES Fortran
  DESCRIPTION "Basic Rocket Motor Simuator"
)

if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
  message(FATAL_ERROR
    "\n"
    "Error: PROJECT_SOURCE_DIR matches PROJECT_BINARY_DIR: ${PROJECT_BINARY_DIR}\n"
    "This project does not support identical build and source paths.\n"
    "Please delete ${PROJECT_SOURCE_DIR}/CMakeCache.txt and ${PROJECT_SOURCE_DIR}/CMakeFiles/"
    "and then build in a different directory.  This may be accomplished in a bash shell by executing\n"
    "  rm -r CMakeCache.txt CMakeFiles/\n"
    "  mkdir build\n"
    "  cd build\n"
    "  export CC=gcc FC=gfortran\n"
    "  cmake .. \n"
    "or by substituting the appropriate syntax for shells other than bash."
  )
endif()

set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
if(MSVC)
   string(APPEND CMAKE_EXE_LINKER_FLAGS " -STACK:10000000000")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -std=f2018")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --standard-semenatics")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -f2018")
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod")

include(CheckFortranSourceCompiles)

check_fortran_source_compiles("
  program main
  integer :: i
  i = 0
  error stop i
  end program
"
  HAVE_NON_CONSTANT_ERROR_STOP
  SRC_EXT ".f90")
if(HAVE_NON_CONSTANT_ERROR_STOP)
  add_definitions(-DHAVE_NON_CONSTANT_ERROR_STOP)
endif()

check_fortran_source_compiles("
  program main
  contains
  pure function foo() result(res)
    error stop 'Error stop is supported in pure functions (F2018)'
  end function
  end program
"
  HAVE_ERROR_STOP_IN_PURE
  SRC_EXT ".f90"
  )
if(HAVE_ERROR_STOP_IN_PURE)
  add_definitions(-DHAVE_ERROR_STOP_IN_PURE)
endif()

foreach(directory src app submodules integration_tests unit_tests)
  add_subdirectory("${directory}")
endforeach()

enable_testing()

add_test( NAME "generate_graphs"
  COMMAND ${CMAKE_BINARY_DIR}/app/generate_graphs.sh
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/app
)

foreach(integration_test
  refurbished-integration
)
  add_test( NAME "${integration_test}"
    COMMAND ${CMAKE_BINARY_DIR}/integration_tests/${integration_test}/test-${integration_test}
  )
  set_tests_properties(${integration_test}
     PROPERTIES PASS_REGULAR_EXPRESSION "All Passed."
  )
endforeach()

foreach(unit_test
  refurbished-unit
)
  add_test( NAME "${unit_test}"
    COMMAND ${CMAKE_BINARY_DIR}/unit_tests/${unit_test}/test-${unit_test}
  )
  set_tests_properties(${unit_test}
     PROPERTIES PASS_REGULAR_EXPRESSION "All Passed."
  )
endforeach()
