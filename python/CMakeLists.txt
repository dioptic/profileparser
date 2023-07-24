cmake_minimum_required(VERSION 3.16)
project(profileparser CXX)

# Default to Release build type
if (NOT CMAKE_BUILD_TYPE OR CMAKE_BUILD_TYPE STREQUAL "")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
endif()

if(WIN32)
  # Prevent pybind11 from sharing resources with other, potentially ABI incompatible modules
  # https://github.com/pybind/pybind11/issues/2898
  add_definitions(-DPYBIND11_COMPILER_TYPE="_${PROJECT_NAME}_abi")
endif()

# Fetch pybind11 library
include(FetchContent)
FetchContent_Declare(
  pybind11
  GIT_REPOSITORY https://github.com/pybind/pybind11
  GIT_TAG        v2.11.1
)
FetchContent_MakeAvailable(pybind11)

# Create python extension module with pybind11
pybind11_add_module(${PROJECT_NAME} "pyprofileparser.cpp")
set_target_properties(${PROJECT_NAME} PROPERTIES
  CXX_STANDARD 20
  CXX_STANDARD_REQUIRED ON
)
install(TARGETS ${PROJECT_NAME} DESTINATION .)

# Compiler options
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
  target_compile_options(${PROJECT_NAME} PUBLIC "/Zc:__cplusplus")
endif()
