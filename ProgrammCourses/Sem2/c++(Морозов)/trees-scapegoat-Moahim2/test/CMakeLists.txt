cmake_minimum_required(VERSION 3.13)

# m_root includes
set(ROOT_INCLUDES ${PROJECT_SOURCE_DIR}/include)
set(TEST_INCLUDES ${PROJECT_SOURCE_DIR}/test/include)

set(PROJECT_NAME trees.test)
project(${PROJECT_NAME})

# Include directories
include_directories(${ROOT_INCLUDES}  ${TEST_INCLUDES})

# Include the gtest library
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

# Source files
file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/src/*.cpp)

# Unit tests

add_executable(runUnitTests src/common/TreeTest.cpp src/ScapegoatTreeTest.cpp)
target_compile_options(runUnitTests PRIVATE ${COMPILE_OPTS} -O3
    -Wno-gnu-zero-variadic-macro-arguments -Wno-unused-function -Wno-missing-braces)
target_link_options(runUnitTests PRIVATE ${LINK_OPTS})
# Standard linking to gtest stuff
target_link_libraries(runUnitTests gtest gtest_main)
# Extra linking for the project
target_link_libraries(runUnitTests trees_lib)
