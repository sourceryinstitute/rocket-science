cmake_minimum_required(VERSION 3.15)

set (VOLFIL_VERSION "1.0.0")
set (ALLOWED_BUILD_TYPES "Debug" "Release")

get_property(multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(multi_config)
  set (CMAKE_CONFIGURATION_TYPES "${ALLOWED_BUILD_TYPES}" CACHE STRING "list of supported configuration types" FORCE)
else()
  set (CMAKE_BUILD_TYPE "Debug" CACHE STRING "Select which configuration to build.")
  set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${ALLOWED_BUILD_TYPES})
endif()

project(volfil
  VERSION "${VOLFILP_VERSION}"
  LANGUAGES Fortran
  DESCRIPTION "Basic Volume Filling Program"
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
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall")
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod")

foreach(directory src tests)
  add_subdirectory("${directory}")
endforeach()

enable_testing()

if(${benchmark})
  message(STATUS "\n\n***\n Setting up benchmark runs: ctest results will be written to files. \n***\n\n" )
  set(write_reference_results "--benchmark")
endif()

# Tests that create individual executable programs
foreach(integration_test
  design
)
  add_test( NAME "${integration_test}"
    COMMAND ${CMAKE_BINARY_DIR}/tests/integration/${integration_test}/test-${integration_test}
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/tests/integration/${integration_test}"
  )
  set_tests_properties(${integration_test}
     PROPERTIES PASS_REGULAR_EXPRESSION "Test passed."
  )
endforeach()
