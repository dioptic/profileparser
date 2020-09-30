cmake_minimum_required(VERSION 3.13)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

project(ProfileParserJs CXX)

if(NOT DEFINED EMSCRIPTEN)
    message(FATAL_ERROR "Project requires EMSCRIPTEN toolchain")
endif()

# ProfileParserJs WASM library

add_executable(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE
    "profile_parser_js.cpp"
    "../cpp/processprofile.h"
    )
target_compile_options(${PROJECT_NAME} PRIVATE
    -Wall -Wextra -Wpedantic
    )
target_include_directories(${PROJECT_NAME} PRIVATE
    "${EMSCRIPTEN_ROOT_PATH}/system/include"
    )
set_target_properties(${PROJECT_NAME} PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
    SUFFIX ".esm.js"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    )

target_link_options(${PROJECT_NAME} PRIVATE
    "--bind" "SHELL:-s WASM=1" "SHELL:-s MODULARIZE=1" "SHELL:-s EXPORT_ES6=1"
    "SHELL:-s EXPORT_NAME=\"${PROJECT_NAME}\""
    "SHELL:-s DISABLE_EXCEPTION_CATCHING=0" "-fexceptions"
    )
target_compile_options(${PROJECT_NAME} PRIVATE
    "SHELL:-s DISABLE_EXCEPTION_CATCHING=0" "-fexceptions"
    )
