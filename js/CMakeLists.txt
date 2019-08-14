cmake_minimum_required(VERSION 3.12)

if(NOT DEFINED ENV{EMSDK})
    message(FATAL_ERROR "EMSDK environment variable not defined")
endif()
set(CMAKE_TOOLCHAIN_FILE "$ENV{EMSDK}/fastcomp/emscripten/cmake/Modules/Platform/Emscripten.cmake")

project(ProfileParserJs CXX)

add_executable(${PROJECT_NAME} "profile_parser_js.cpp")

target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_17)

target_include_directories(${PROJECT_NAME} PRIVATE "$ENV{EMSDK}/fastcomp/emscripten/system/include")

set_target_properties(
    ${PROJECT_NAME}
    PROPERTIES
        LINK_FLAGS "--bind -s WASM=1 -s DISABLE_EXCEPTION_CATCHING=0 -s MODULARIZE=1 -s EXPORT_NAME=\"${PROJECT_NAME}\""
)

