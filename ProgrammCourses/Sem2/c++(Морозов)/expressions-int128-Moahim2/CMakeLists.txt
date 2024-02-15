cmake_minimum_required(VERSION 3.13)

include(test/Strict.cmake)

set(PROJECT_NAME expressions)
project(${PROJECT_NAME})

# Set up the compiler flags
set(CMAKE_CXX_FLAGS "-g")
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Include directories
set(COMMON_INCLUDES ${PROJECT_SOURCE_DIR}/include)
include_directories(${COMMON_INCLUDES})

# Source files
file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/src/*.cpp)

# Compile source files into a library
add_library(expressions_lib ${SRC_FILES})
target_compile_options(expressions_lib PUBLIC ${COMPILE_OPTS})
target_link_options(expressions_lib PUBLIC ${LINK_OPTS})
setup_warnings(expressions_lib)

# Main is separate
add_executable(expressions ${PROJECT_SOURCE_DIR}/src/main.cpp)
target_compile_options(expressions PRIVATE ${COMPILE_OPTS})
target_link_options(expressions PRIVATE ${LINK_OPTS})
target_link_libraries(expressions expressions_lib)
setup_warnings(expressions)

# google test is a git submodule
add_subdirectory(googletest)

enable_testing()

# test is a git submodule
add_subdirectory(test)
