cmake_minimum_required(VERSION 3.13)

# root includes
set(ROOT_INCLUDES ${PROJECT_SOURCE_DIR}/include)

# test includes
set(TEST_INCLUDES ${PROJECT_SOURCE_DIR}/test/include)
list(APPEND TEST_INCLUDES ${PROJECT_SOURCE_DIR}/test/common/include)

set(PROJECT_NAME expressions_test)
project(${PROJECT_NAME})

# Include directories
include_directories(${ROOT_INCLUDES} ${TEST_INCLUDES})

# Source files
file(GLOB_RECURSE SRC_FILES FOLLOW_SYMLINKS ${PROJECT_SOURCE_DIR}/**.cpp)

# Include the gtest library
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

# Unit tests
add_executable(runUnitTests ${SRC_FILES})
target_compile_options(runUnitTests PRIVATE ${COMPILE_OPTS} -O3 -Wno-gnu-zero-variadic-macro-arguments)
target_link_options(runUnitTests PRIVATE ${LINK_OPTS})

# Standard linking to gtest stuff
target_link_libraries(runUnitTests gtest gtest_main)

# Extra linking for the project
target_link_libraries(runUnitTests expressions_lib)
