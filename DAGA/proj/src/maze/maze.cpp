#include "maze.h"

#include <random>
#include <stdexcept>

namespace our {

cell::cell() : wall_direction_mask(15), in_use(0), x_cord(0), y_cord(0) {}

cell::cell(unsigned char C_wall_direction_mask, int C_in_use, int C_x_cord, int C_y_cord)
{
    wall_direction_mask = C_wall_direction_mask;
    in_use = C_in_use;
    x_cord = C_x_cord;
    y_cord = C_y_cord;
}

Maze::Maze(int i_length, int i_width)
{
    initialization(i_length, i_width);
    start_cell_cords = get_random_start_point();
    end_cell_cords = get_random_end_point(start_cell_cords);
}

int Maze::initialization(int i_length, int i_width)
{
    if (i_length <= 0 || i_width <= 0)
    {
        throw std::invalid_argument("Недопустимые размеры");
        return 0;
    }
    length = i_length;
    width = i_width;
    cell_array.resize(length, std::vector<cell>(width));
    for (int x = 0; x < length; ++x)
    {
        for (int y = 0; y < width; ++y)
        {
            cell_array[x][y] = cell(15, false, x, y);
        }
    }
    return 1;
}

std::pair<int, int> Maze::get_random_start_point()
{
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dist;
    dist = std::uniform_int_distribution<>(0, 3);
    int side = dist(gen);
    int x, y;

    switch (side)
    {
        case 0:
            x = 0;
            dist = std::uniform_int_distribution<>(0, width - 1);
            y = dist(gen);
            break;
        case 1:
            x = length - 1;
            dist = std::uniform_int_distribution<>(0, width - 1);
            y = dist(gen);
            break;
        case 2:
            y = 0;
            dist = std::uniform_int_distribution<>(0, length - 1);
            x = dist(gen);
            break;
        case 3:
            y = width - 1;
            dist = std::uniform_int_distribution<>(0, length - 1);
            x = dist(gen);
            break;
    }

    return std::make_pair(x, y);
}

std::pair<int, int> Maze::get_random_end_point(const std::pair<int, int>& start)
{
    std::pair<int, int> end;
    do
    {
        end = get_random_start_point();
    } while (end == start);
    return end;
}

bool Maze::isValid(int x, int y)
{
    return x >= 0 && x < length && y >= 0 && y < width;
}

} // namespace our
