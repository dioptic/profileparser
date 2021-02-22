cmake_minimum_required(VERSION 3.16)

project(ProfileParser)

option(BUILD_PYTHON "Build python module" ON)
option(BUILD_JS "Build JS/WASM module" OFF)

if (BUILD_PYTHON)
    message(STATUS "Enable python module")
    add_subdirectory(python)
endif()

if (BUILD_JS)
    message(STATUS "Enable javascript module")
    message(FATAL_ERROR "Due to CMake limitations the JS module must be build separately for now")
endif()
