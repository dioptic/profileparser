cmake_minimum_required(VERSION 3.12)
project(pyprofileparser CXX)

# Find Python3 and its development components
find_package (Python3 COMPONENTS Interpreter Development)
if(NOT Python3_FOUND)
  message(FATAL_ERROR "Python3 components not found")
endif()

# Determine python extension suffix for this platform
exec_program(${Python3_EXECUTABLE}
  ARGS -c "\"import importlib; print(importlib.machinery.EXTENSION_SUFFIXES[0])\""
  OUTPUT_VARIABLE Python3_EXTSUFFIX)

# Create python extension module
Python3_add_library(${PROJECT_NAME} MODULE "pyprofileparser.cpp")
target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_17)
set_target_properties(${PROJECT_NAME} PROPERTIES PREFIX "" SUFFIX ${Python3_EXTSUFFIX} OUTPUT_NAME "${PROJECT_NAME}")

