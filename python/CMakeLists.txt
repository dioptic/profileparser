cmake_minimum_required(VERSION 3.16)
project(profileparser CXX)

find_package(Python 3.9 REQUIRED COMPONENTS Interpreter Development.Module OPTIONAL_COMPONENTS Development.SABIModule)

# Use LTO for building this module
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)

# Fetch nanobind library
include(FetchContent)
FetchContent_Declare(
  nanobind
  GIT_REPOSITORY https://github.com/wjakob/nanobind
  GIT_TAG        0035bc43fa2af390b48608c9bf4595d7505c2386
)
FetchContent_MakeAvailable(nanobind)

# Create python extension module with nanobind
nanobind_add_module(${PROJECT_NAME}
  STABLE_ABI
  "pyprofileparser.cpp"
)

set_target_properties(${PROJECT_NAME} PROPERTIES
  CXX_STANDARD 20
  CXX_STANDARD_REQUIRED ON
)

add_subdirectory("../cpp" "${CMAKE_CURRENT_BINARY_DIR}/cpp")
target_link_libraries(${PROJECT_NAME} PUBLIC
  libProfileParser::libProfileParser
)

nanobind_add_stub(
  ${PROJECT_NAME}_stub
  MODULE ${PROJECT_NAME}
  OUTPUT ${PROJECT_NAME}.pyi
  PYTHON_PATH $<TARGET_FILE_DIR:${PROJECT_NAME}>
  DEPENDS ${PROJECT_NAME}
  MARKER_FILE py.typed
)

install(TARGETS ${PROJECT_NAME}
  DESTINATION "dioptic/profileparser"
)
install(FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/__init__.py
  ${${PROJECT_NAME}_BINARY_DIR}/py.typed
  ${${PROJECT_NAME}_BINARY_DIR}/${PROJECT_NAME}.pyi
  DESTINATION "dioptic/profileparser"
)

# Compiler options
if(MSVC)
  target_compile_options(${PROJECT_NAME} PUBLIC /W3 /permissive- /Zc:__cplusplus)
else()
  target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra)
endif()
