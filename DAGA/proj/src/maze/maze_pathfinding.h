#pragma once

#include "maze.h"

#include <utility>
#include <vector>

namespace our {

std::vector<std::pair<int, int>> find_shortest_path(MazeSync& maze,
                                                    std::pair<int, int> start,
                                                    std::pair<int, int> end);

void print_maze(const Maze& maze);
void print_maze_with_path(const MazeSync& maze, const std::vector<std::pair<int, int>>& path);
void print_path_coordinates(const std::vector<std::pair<int, int>>& path);
void print_maze_info(const MazeSync& maze);

} // namespace our
