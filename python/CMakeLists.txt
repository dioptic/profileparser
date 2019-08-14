cmake_minimum_required(VERSION 3.12)

project(pyprofileparser CXX)

find_package(PythonInterp 3.6 REQUIRED)
find_package(PythonLibs 3.6 REQUIRED)
exec_program("${PYTHON_EXECUTABLE}-config" ARGS "--extension-suffix" OUTPUT_VARIABLE PY_EXTSUFFIX)

add_library(${PROJECT_NAME} SHARED "pyprofileparser.cpp")

target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_17)

target_include_directories(${PROJECT_NAME} PRIVATE ${PYTHON_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} ${PYTHON_LIBRARIES})

set_target_properties(
    ${PROJECT_NAME}
    PROPERTIES
        PREFIX ""
        SUFFIX ${PY_EXTSUFFIX}
        OUTPUT_NAME "${PROJECT_NAME}"
)
