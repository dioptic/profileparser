cmake_minimum_required(VERSION 3.24)

if(NOT SKBUILD)
  message(FATAL_ERROR "Build should be driven by scikit-build-core")
endif()
set(MODULE_NAME "_profileparser")
set(MODULE_VERSION ${SKBUILD_PROJECT_VERSION})
project(${MODULE_NAME} VERSION ${MODULE_VERSION} LANGUAGES CXX)

find_package(
  Python 3.12 REQUIRED
  COMPONENTS Interpreter Development.Module
  OPTIONAL_COMPONENTS Development.SABIModule
)
find_package(nanobind CONFIG REQUIRED)

# Create python extension module with nanobind
nanobind_add_module(${MODULE_NAME}
  STABLE_ABI
  NB_STATIC
  "pyprofileparser.cpp"
)

set_target_properties(${MODULE_NAME} PROPERTIES
  CXX_STANDARD 20
  CXX_STANDARD_REQUIRED ON
)
target_compile_definitions(${MODULE_NAME} PRIVATE MODULE_NAME=${MODULE_NAME})
target_compile_definitions(${MODULE_NAME} PRIVATE MODULE_VERSION=${MODULE_VERSION})

add_subdirectory("../cpp" "${CMAKE_CURRENT_BINARY_DIR}/cpp")
target_link_libraries(${MODULE_NAME} PUBLIC
  libProfileParser::libProfileParser
)

nanobind_add_stub(
  ${MODULE_NAME}_stub
  MODULE ${MODULE_NAME}
  OUTPUT ${MODULE_NAME}.pyi
  PYTHON_PATH $<TARGET_FILE_DIR:${MODULE_NAME}>
  DEPENDS ${MODULE_NAME}
  MARKER_FILE py.typed
)

install(TARGETS ${MODULE_NAME}
  DESTINATION "dioptic/profileparser"
)
install(FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/__init__.py
  ${${PROJECT_NAME}_BINARY_DIR}/py.typed
  ${${PROJECT_NAME}_BINARY_DIR}/${MODULE_NAME}.pyi
  DESTINATION "dioptic/profileparser"
)

# Compiler options
if(MSVC)
  target_compile_options(${MODULE_NAME} PUBLIC /W3 /permissive- /Zc:__cplusplus)
else()
  target_compile_options(${MODULE_NAME} PRIVATE -Wall -Wextra)
endif()
