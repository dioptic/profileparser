cmake_minimum_required(VERSION 3.12)
set(CMAKE_CXX_STANDARD 17)

project(profile_parser)

find_package(PythonInterp 3.6 REQUIRED)
find_package(PythonLibs 3.6 REQUIRED)
exec_program("${PYTHON_EXECUTABLE}-config" ARGS "--extension-suffix" OUTPUT_VARIABLE PY_EXTSUFFIX)

add_library(${PROJECT_NAME} SHARED "profile_parser_py.cpp")

target_include_directories(${PROJECT_NAME} PRIVATE ${PYTHON_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} ${PYTHON_LIBRARIES})

set_target_properties(
    ${PROJECT_NAME}
    PROPERTIES
        PREFIX ""
        SUFFIX ${PY_EXTSUFFIX}
        OUTPUT_NAME "${PROJECT_NAME}"
)
