#include "maze_pathfinding.h"

#include <algorithm>
#include <iostream>
#include <queue>
#include <unordered_map>

namespace our {

std::vector<std::pair<int, int>> find_shortest_path(MazeSync& maze, std::pair<int, int> start, std::pair<int, int> end)
{
    if (!maze.isValid(start.first, start.second) || !maze.isValid(end.first, end.second)) {
        throw std::invalid_argument("Start or end point is outside the maze.");
    }

    std::vector<std::vector<int>> distance(maze.length, std::vector<int>(maze.width, -1));
    std::queue<std::pair<int, int>> queue;
    std::unordered_map<int, std::unordered_map<int, std::pair<int, int>>> prev;

    distance[start.first][start.second] = 0;
    queue.push(start);

    int dx[] = { -1, 1, 0, 0 };
    int dy[] = { 0, 0, -1, 1 };

    while (!queue.empty()) {
        auto current = queue.front();
        queue.pop();

        if (current == end) {
            break;
        }

        for (int i = 0; i < 4; ++i) {
            int nx = current.first + dx[i];
            int ny = current.second + dy[i];

            if (maze.isValid(nx, ny) && distance[nx][ny] == -1) {
                bool can_move = false;
                switch (i) {
                case 0: can_move = !(maze.cell_array[current.first][current.second].wall_direction_mask & our::Up); break;
                case 1: can_move = !(maze.cell_array[current.first][current.second].wall_direction_mask & our::Down); break;
                case 2: can_move = !(maze.cell_array[current.first][current.second].wall_direction_mask & our::Left); break;
                case 3: can_move = !(maze.cell_array[current.first][current.second].wall_direction_mask & our::Right); break;
                }

                if (can_move) {
                    distance[nx][ny] = distance[current.first][current.second] + 1;
                    queue.push({ nx, ny });
                    prev[nx][ny] = current;
                }
            }
        }
    }

    std::vector<std::pair<int, int>> path;
    if (distance[end.first][end.second] != -1) {
        std::pair<int, int> current = end;
        while (current != start) {
            path.push_back(current);
            current = prev[current.first][current.second];
        }
        path.push_back(start);
        std::reverse(path.begin(), path.end());
    }

    return path;
}

void print_maze(const Maze& maze)
{
    for (int x = 0; x < maze.length; ++x) {
        for (int y = 0; y < maze.width; ++y) {
            std::cout << "+";
            if (maze.cell_array[x][y].wall_direction_mask & our::Up) {
                std::cout << "---";
            }
            else {
                std::cout << "   ";
            }
        }
        std::cout << "+" << std::endl;

        for (int y = 0; y < maze.width; ++y) {
            if (maze.cell_array[x][y].wall_direction_mask & our::Left) {
                std::cout << "|";
            }
            else {
                std::cout << " ";
            }

            if (x == maze.start_cell_cords.first && y == maze.start_cell_cords.second) {
                std::cout << " S ";
            }
            else if (x == maze.end_cell_cords.first && y == maze.end_cell_cords.second) {
                std::cout << " E ";
            }
            else {
                std::cout << "   ";
            }
        }
        std::cout << "|" << std::endl;
    }

    for (int y = 0; y < maze.width; ++y) {
        std::cout << "+---";
    }
    std::cout << "+" << std::endl;
}

void print_maze_with_path(const MazeSync& maze, const std::vector<std::pair<int, int>>& path)
{
    std::vector<std::vector<char>> display(maze.length, std::vector<char>(maze.width, ' '));

    for (const auto& p : path) {
        display[p.first][p.second] = '*';
    }

    for (int x = 0; x < maze.length; ++x) {
        for (int y = 0; y < maze.width; ++y) {
            std::cout << "+";
            if (maze.cell_array[x][y].wall_direction_mask & our::Up) {
                std::cout << "---";
            }
            else {
                std::cout << "   ";
            }
        }
        std::cout << "+" << std::endl;

        for (int y = 0; y < maze.width; ++y) {
            if (maze.cell_array[x][y].wall_direction_mask & our::Left) {
                std::cout << "|";
            }
            else {
                std::cout << " ";
            }

            if (x == maze.start_cell_cords.first && y == maze.start_cell_cords.second) {
                std::cout << " S ";
            }
            else if (x == maze.end_cell_cords.first && y == maze.end_cell_cords.second) {
                std::cout << " E ";
            }
            else if (display[x][y] == '*') {
                std::cout << " * ";
            }
            else {
                std::cout << "   ";
            }
        }
        std::cout << "|" << std::endl;
    }

    for (int y = 0; y < maze.width; ++y) {
        std::cout << "+---";
    }
    std::cout << "+" << std::endl;
}

void print_path_coordinates(const std::vector<std::pair<int, int>>& path)
{
    if (path.empty()) {
        std::cout << "Path not found!" << std::endl;
        return;
    }

    std::cout << "Path coordinates:" << std::endl;
    for (const auto& p : path) {
        std::cout << "(" << p.first << ", " << p.second << ") ";
    }
    std::cout << std::endl;
}

void print_maze_info(const MazeSync& maze)
{
    std::cout << "Maze information:" << std::endl;
    std::cout << "Start: (" << maze.start_cell_cords.first << ", " << maze.start_cell_cords.second << ")" << std::endl;
    std::cout << "End: (" << maze.end_cell_cords.first << ", " << maze.end_cell_cords.second << ")" << std::endl;

    for (int x = 0; x < maze.length; ++x) {
        for (int y = 0; y < maze.width; ++y) {
            std::cout << "Cell [" << x << ", " << y << "]: "
                      << "walls = " << +maze.cell_array[x][y].wall_direction_mask
                      << ", in_use = " << maze.cell_array[x][y].in_use << std::endl;
        }
    }
}

} // namespace our
