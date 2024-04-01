cmake_minimum_required(VERSION 3.13)

project(ProfileParserJs CXX)

if(NOT DEFINED EMSCRIPTEN)
    message(FATAL_ERROR "Project requires EMSCRIPTEN toolchain")
endif()

# ProfileParserJs WASM library

add_executable(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE
    "profile_parser_js.cpp"
    "../cpp/processprofile.hpp"
    )
target_compile_options(${PROJECT_NAME} PRIVATE
    -Wall -Wextra -Wpedantic
    )
set_target_properties(${PROJECT_NAME} PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
    SUFFIX ".mjs"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    )

target_link_options(${PROJECT_NAME} PRIVATE
    "-lembind"
    "--no-entry"
    "-sWASM=1"
    "-sMODULARIZE=1"
    "-sEXPORT_ES6=1"
    "-sENVIRONMENT=web,worker"
    "-sEXPORT_NAME=\"${PROJECT_NAME}\""
    "--embind-emit-tsd=${PROJECT_NAME}.d.ts"
    "-fwasm-exceptions"
    )
target_compile_options(${PROJECT_NAME} PRIVATE
    "-fwasm-exceptions"
    )
