cmake_minimum_required(VERSION 3.16)
project(libProfileParser CXX)

include(FetchContent)
FetchContent_Declare(
    peglib
    GIT_REPOSITORY https://github.com/yhirose/cpp-peglib
    GIT_TAG 4b3bd64
    GIT_SHALLOW TRUE
)
FetchContent_GetProperties(peglib)
if(NOT peglib_POPULATED)
    FetchContent_Populate(peglib)
endif()

add_library(libProfileParser STATIC)
add_library(libProfileParser::libProfileParser ALIAS libProfileParser)

target_include_directories(libProfileParser PUBLIC
    "${CMAKE_CURRENT_LIST_DIR}/include"
    )
target_include_directories(libProfileParser PRIVATE
    "${peglib_SOURCE_DIR}"
    )

set_target_properties(libProfileParser PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
    POSITION_INDEPENDENT_CODE ON
    )

if(MSVC)
    target_compile_options(libProfileParser PUBLIC /W3 /permissive- /Zc:__cplusplus)
else()
    target_compile_options(libProfileParser PRIVATE -Wall -Wextra -Wpedantic)
endif()

target_sources(libProfileParser PRIVATE
    "include/profileparser.hpp"
    "include/profileparser/types.hpp"
    "include/profileparser/utils.hpp"
    "lineinfo.cpp"
    "parser.cpp"
    "parser.hpp"
    "printers.cpp"
    "validation.cpp"
    )
