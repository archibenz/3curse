cmake_minimum_required(VERSION 3.16)
project(maze_gui LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_AUTOMOC ON)

find_package(Qt6 REQUIRED COMPONENTS Widgets)
find_package(Threads REQUIRED)

set(SOURCES
    src/maze/maze.cpp
    src/maze/maze_generation.cpp
    src/maze/maze_multithreading.cpp
    src/maze/maze_pathfinding.cpp
    src/maze/maze_gui.cpp
)

add_executable(maze_gui ${SOURCES})

target_link_libraries(maze_gui PRIVATE Qt6::Widgets Threads::Threads)
