cmake_minimum_required(VERSION 3.13)

# root includes
set(ROOT_INCLUDES ${PROJECT_SOURCE_DIR}/include)

set(PROJECT_NAME lfru-multi-type-test)
project(${PROJECT_NAME})

if (NOT SANITIZE)
# Jemalloc
find_package(PkgConfig REQUIRED)
pkg_check_modules(JEMALLOC REQUIRED IMPORTED_TARGET jemalloc)

include_directories(${JEMALLOC_INCLUDE_DIRS})
endif()

# Inlcude directories
include_directories(${ROOT_INCLUDES})

# Source files
file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/src/*.cpp)

# Unit tests
add_executable(runUnitTests ${SRC_FILES})
target_compile_options(runUnitTests PRIVATE ${COMPILE_OPTS} -O1 -Wno-gnu-zero-variadic-macro-arguments -Wno-unused-function)
target_link_options(runUnitTests PRIVATE ${LINK_OPTS})

# Standard linking to gtest stuff
target_link_libraries(runUnitTests gtest gtest_main)

# Extra linking for the project
target_link_libraries(runUnitTests lfru_multi_type_lib)
if (NOT SANITIZE)
target_link_libraries(runUnitTests PkgConfig::JEMALLOC)
endif()
