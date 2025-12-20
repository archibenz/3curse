#include "maze.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <random>
#include <stack>
#include <thread>
#include <unordered_set>

namespace our {

Thread_sync::Thread_sync(std::vector<std::thread>* t_threads)
{
    threads = t_threads;
    for (std::size_t i = 1; i <= threads->size(); ++i)
    {
        flag.emplace_back(false);
        identifier.emplace_back(i);
    }
    const std::size_t level_count = 2;
    for (std::size_t i = 0; i < level_count; ++i)
    {
        barriers.emplace_back();
        barriers.back().participants = threads->size();
    }
}

bool Thread_sync::get_flag(std::size_t index)
{
    std::lock_guard<std::mutex> lock(mtx);
    return flag[index - 1];
}

void Thread_sync::setFlag(std::size_t index)
{
    std::lock_guard<std::mutex> lock(mtx);
    flag[index - 1] = true;
}

void Thread_sync::barrier_wait(BarrierState& barrier)
{
    std::unique_lock<std::mutex> lock(barrier.mtx);
    if (barrier.participants == 0)
    {
        return;
    }
    std::size_t gen = barrier.generation;
    ++barrier.arrived;
    if (barrier.arrived == barrier.participants)
    {
        barrier.generation++;
        barrier.arrived = 0;
        barrier.cv.notify_all();
        return;
    }
    barrier.cv.wait(lock, [&barrier, gen]() { return gen != barrier.generation; });
}

void Thread_sync::barrier_drop(BarrierState& barrier)
{
    std::unique_lock<std::mutex> lock(barrier.mtx);
    if (barrier.participants == 0)
    {
        return;
    }
    std::size_t expected = barrier.participants;
    ++barrier.arrived;
    barrier.participants--;
    if (barrier.arrived == expected)
    {
        barrier.generation++;
        barrier.arrived = 0;
        barrier.cv.notify_all();
    }
}

void Thread_sync::arrive_and_wait(std::size_t level)
{
    if (level >= barriers.size())
    {
        return;
    }
    barrier_wait(barriers[level]);
}

void Thread_sync::arrive_and_drop(std::size_t level)
{
    if (level >= barriers.size())
    {
        return;
    }
    barrier_drop(barriers[level]);
}

void Thread_sync::run()
{
}

MazeSync::MazeSync(int i_length, int i_width, int i_mutex_cell_size)
    : Maze(i_length, i_width), mutex_cell_size(i_mutex_cell_size)
{
    initialize_mutexes();
}

int MazeSync::calculate_mutex_count()
{
    if (mutex_cell_size <= 0)
    {
        throw std::invalid_argument("Размер ячейки мьютекса должен быть положительным.");
    }
    int horizontal_mutex_count = std::ceil(static_cast<double>(length) / mutex_cell_size);
    int vertical_mutex_count = std::ceil(static_cast<double>(width) / mutex_cell_size);
    return horizontal_mutex_count * vertical_mutex_count;
}

void MazeSync::initialize_mutexes()
{
    int mutex_count = calculate_mutex_count();
    mutexes = std::vector<std::mutex>(mutex_count);
}

int MazeSync::get_mutex_index(int x, int y)
{
    if (mutex_cell_size <= 0)
    {
        throw std::invalid_argument("Размер ячейки мьютекса должен быть положительным.");
    }
    if (x < 0 || x >= length || y < 0 || y >= width)
    {
        throw std::out_of_range("Координаты выходят за границы лабиринта.");
    }
    int mutex_x = x / mutex_cell_size;
    int mutex_y = y / mutex_cell_size;
    int horizontal_mutex_count = std::ceil(static_cast<double>(length) / mutex_cell_size);
    return mutex_x + mutex_y * horizontal_mutex_count;
}

bool MazeSync::try_lock_cell(int x, int y, std::unordered_set<int>& locked_mutexes)
{
    int mutex_index = get_mutex_index(x, y);
    locked_mutexes.insert(mutex_index);
    return mutexes[mutex_index].try_lock();
}

void MazeSync::lock_mutex(int x, int y)
{
    int mutex_index = get_mutex_index(x, y);
    mutexes[mutex_index].lock();
}

void MazeSync::unlock_mutex(int x, int y)
{
    int mutex_index = get_mutex_index(x, y);
    mutexes[mutex_index].unlock();
}

bool MazeSync::try_lock_mutex_with_timeout(int x, int y, std::unordered_set<int>& locked_mutexes)
{
    auto start_time = std::chrono::steady_clock::now();
    const std::chrono::milliseconds timeout(6);

    while (true)
    {
        if (try_lock_cell(x, y, locked_mutexes))
        {
            return true;
        }

        auto current_time = std::chrono::steady_clock::now();
        if (std::chrono::duration_cast<std::chrono::milliseconds>(current_time - start_time) >= timeout)
        {
            return false;
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
}

bool MazeSync::is_too_close(const std::pair<int, int>& p1, const std::pair<int, int>& p2)
{
    return (std::abs(p1.first - p2.first) <= 1 && p1.second == p2.second) ||
           (std::abs(p1.second - p2.second) <= 1 && p1.first == p2.first);
}

std::vector<std::pair<int, int>> MazeSync::generate_random_start_points(int num_points)
{
    if (num_points <= 0)
    {
        throw std::invalid_argument("Количество точек должно быть положительным.");
    }

    if (num_points > std::max(length, width))
    {
        throw std::runtime_error("Слишком много точек, в лабиринте такого размера нет столько места.");
    }

    std::vector<std::pair<int, int>> start_points;

    while (start_points.size() < static_cast<std::size_t>(num_points))
    {
        std::pair<int, int> new_point = get_random_start_point();
        bool too_close = false;

        for (const auto& point : start_points)
        {
            if (is_too_close(new_point, point))
            {
                too_close = true;
                break;
            }
        }

        if (!too_close)
        {
            start_points.push_back(new_point);
        }
    }

    return start_points;
}

void MazeSync::set_random_start_end_points(std::vector<std::pair<int, int>> points)
{
    if (points.size() < 2)
    {
        throw std::invalid_argument("Необходимы две стартовые точки.");
    }

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dist(0, points.size() - 1);
    int start_index = dist(gen);
    int end_index;

    do
    {
        end_index = dist(gen);
    } while (end_index == start_index);

    start_cell_cords = points[start_index];
    end_cell_cords = points[end_index];
}

void MazeSync::generate_and_set_random_start_end_points(std::vector<std::pair<int, int>>& extreme_points)
{
    if (mutex_cell_size <= 0)
    {
        throw std::invalid_argument("Размер мьютексной ячейки должен быть положительным");
    }

    std::vector<int> unique_mutex_indices;
    int horizontal_mutex_count = std::ceil(static_cast<double>(length) / mutex_cell_size);
    int vertical_mutex_count = std::ceil(static_cast<double>(width) / mutex_cell_size);

    for (int y = 0; y < vertical_mutex_count; y++)
    {
        unique_mutex_indices.push_back(get_mutex_index(0, y * mutex_cell_size));
        unique_mutex_indices.push_back(get_mutex_index(length - 1, y * mutex_cell_size));
    }
    for (int x = 1; x < horizontal_mutex_count - 1; x++)
    {
        unique_mutex_indices.push_back(get_mutex_index(x * mutex_cell_size, 0));
        unique_mutex_indices.push_back(get_mutex_index(x * mutex_cell_size, width - 1));
    }

    std::sort(unique_mutex_indices.begin(), unique_mutex_indices.end());
    unique_mutex_indices.erase(std::unique(unique_mutex_indices.begin(), unique_mutex_indices.end()), unique_mutex_indices.end());

    if (unique_mutex_indices.size() < 2)
    {
        throw std::runtime_error("Не хватает крайних ячеек.");
    }

    extreme_points.clear();
    for (int mutex_index : unique_mutex_indices)
    {
        int mutex_x = (mutex_index % horizontal_mutex_count) * mutex_cell_size;
        int mutex_y = (mutex_index / horizontal_mutex_count) * mutex_cell_size;

        extreme_points.emplace_back(mutex_x, mutex_y);
        extreme_points.emplace_back(mutex_x + std::min(mutex_cell_size, length - mutex_x) - 1,
                                    mutex_y + std::min(mutex_cell_size, width - mutex_y) - 1);
    }

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distrib(0, extreme_points.size() - 1);

    int startIndex = distrib(gen);
    int endIndex;

    do
    {
        endIndex = distrib(gen);
    } while (endIndex == startIndex);

    start_cell_cords = extreme_points[startIndex];
    end_cell_cords = extreme_points[endIndex];
}

unsigned char MazeSync::check_another_thread_identifier(std::size_t thread_identifier,
                                                        std::size_t another_thread_identifier,
                                                        Thread_sync& thread_sync_pointer,
                                                        Directions direction)
{
    if (another_thread_identifier == 0)
    {
        return direction;
    }

    if (thread_sync_pointer.get_flag(another_thread_identifier))
    {
        return 0;
    }

    if (thread_identifier == another_thread_identifier)
    {
        return direction;
    }

    return 0;
}

unsigned char MazeSync::multithread_get_available_neighbors(int x,
                                                             int y,
                                                             std::size_t thread_identifier,
                                                             Thread_sync& thread_sync_pointer,
                                                             std::unordered_set<int>& locked_mutexes)
{
    unsigned char available_directions = 0;
    unsigned char failed_try_lock_mutex = 0;
    std::size_t neighbour_cell_thread_identifier;

    int current_cell_mutex_index = get_mutex_index(x, y);
    locked_mutexes.insert(current_cell_mutex_index);

    if (isValid(x - 1, y))
    {
        int neighbour_cell_mutex_index = get_mutex_index(x - 1, y);
        if (neighbour_cell_mutex_index == current_cell_mutex_index)
        {
            neighbour_cell_thread_identifier = cell_array[x - 1][y].in_use;
            available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Up);
        }
        else if ((locked_mutexes.find(neighbour_cell_mutex_index) == locked_mutexes.end()))
        {
            if (try_lock_cell(x - 1, y, locked_mutexes) && (cell_array[x - 1][y].in_use != thread_identifier))
            {
                neighbour_cell_thread_identifier = cell_array[x - 1][y].in_use;
                available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Up);
            }
            else
            {
                failed_try_lock_mutex |= Up;
            }
        }
    }

    if (isValid(x + 1, y))
    {
        int neighbour_cell_mutex_index = get_mutex_index(x + 1, y);
        if (neighbour_cell_mutex_index == current_cell_mutex_index)
        {
            neighbour_cell_thread_identifier = cell_array[x + 1][y].in_use;
            available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Down);
        }
        else if ((locked_mutexes.find(neighbour_cell_mutex_index) == locked_mutexes.end()))
        {
            if (try_lock_cell(x + 1, y, locked_mutexes) && (cell_array[x + 1][y].in_use != thread_identifier))
            {
                neighbour_cell_thread_identifier = cell_array[x + 1][y].in_use;
                available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Down);
            }
            else
            {
                failed_try_lock_mutex |= Down;
            }
        }
    }

    if (isValid(x, y - 1))
    {
        int neighbour_cell_mutex_index = get_mutex_index(x, y - 1);
        if (neighbour_cell_mutex_index == current_cell_mutex_index)
        {
            neighbour_cell_thread_identifier = cell_array[x][y - 1].in_use;
            available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Left);
        }
        else if ((locked_mutexes.find(neighbour_cell_mutex_index) == locked_mutexes.end()))
        {
            if (try_lock_cell(x, y - 1, locked_mutexes) && (cell_array[x][y - 1].in_use != thread_identifier))
            {
                neighbour_cell_thread_identifier = cell_array[x][y - 1].in_use;
                available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Left);
            }
            else
            {
                failed_try_lock_mutex |= Left;
            }
        }
    }

    if (isValid(x, y + 1))
    {
        int neighbour_cell_mutex_index = get_mutex_index(x, y + 1);
        if (neighbour_cell_mutex_index == current_cell_mutex_index)
        {
            neighbour_cell_thread_identifier = cell_array[x][y + 1].in_use;
            available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Right);
        }
        else if ((locked_mutexes.find(neighbour_cell_mutex_index) == locked_mutexes.end()))
        {
            if (try_lock_cell(x, y + 1, locked_mutexes) && (cell_array[x][y + 1].in_use != thread_identifier))
            {
                neighbour_cell_thread_identifier = cell_array[x][y + 1].in_use;
                available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Right);
            }
            else
            {
                failed_try_lock_mutex |= Right;
            }
        }
    }

    if (failed_try_lock_mutex == 0)
    {
        return available_directions;
    }

    if (failed_try_lock_mutex & Up)
    {
        if (try_lock_mutex_with_timeout(x - 1, y, locked_mutexes) && (cell_array[x - 1][y].in_use != thread_identifier))
        {
            neighbour_cell_thread_identifier = cell_array[x - 1][y].in_use;
            available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Up);
        }
    }
    if (failed_try_lock_mutex & Down)
    {
        if (try_lock_mutex_with_timeout(x + 1, y, locked_mutexes) && (cell_array[x + 1][y].in_use != thread_identifier))
        {
            neighbour_cell_thread_identifier = cell_array[x + 1][y].in_use;
            available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Down);
        }
    }
    if (failed_try_lock_mutex & Left)
    {
        if (try_lock_mutex_with_timeout(x, y - 1, locked_mutexes) && (cell_array[x][y - 1].in_use != thread_identifier))
        {
            neighbour_cell_thread_identifier = cell_array[x][y - 1].in_use;
            available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Left);
        }
    }
    if (failed_try_lock_mutex & Right)
    {
        if (try_lock_mutex_with_timeout(x, y + 1, locked_mutexes) && (cell_array[x][y + 1].in_use != thread_identifier))
        {
            neighbour_cell_thread_identifier = cell_array[x][y + 1].in_use;
            available_directions |= check_another_thread_identifier(thread_identifier, neighbour_cell_thread_identifier, thread_sync_pointer, Right);
        }
    }

    return available_directions;
}

void MazeSync::unlock_all_mutexes(std::unordered_set<int>& locked_mutexes)
{
    for (int mutex_index : locked_mutexes)
    {
        mutexes[mutex_index].unlock();
    }
    locked_mutexes.clear();
}

void MazeSync::generate_multithread_imperfect(int extra_loops, int num_threads)
{
    auto start_time = std::chrono::high_resolution_clock::now();

    std::vector<std::thread> threads;
    threads.reserve(num_threads);
    Thread_sync ts(&threads);

    std::vector<std::pair<int, int>> start_points;
    start_points.push_back(start_cell_cords);

    std::vector<std::pair<int, int>> corners = {
        {0, 0},
        {0, width - 1},
        {length - 1, 0},
        {length - 1, width - 1}
    };

    corners.erase(
        std::remove(corners.begin(), corners.end(), start_cell_cords),
        corners.end());

    for (auto& corner : corners)
    {
        if (start_points.size() < static_cast<std::size_t>(num_threads))
        {
            start_points.push_back(corner);
        }
    }

    int threads_to_spawn = std::min(num_threads, static_cast<int>(start_points.size()));
    int loops_per_thread = extra_loops / threads_to_spawn;

    for (int i = 0; i < threads_to_spawn; ++i)
    {
        auto start = start_points[i];
        std::size_t tid = i + 1;
        threads.emplace_back([this, loops_per_thread, start, tid, &ts]() {
            int local_loops = loops_per_thread;
            std::stack<cell*> st;
            lock_mutex(start.first, start.second);
            cell_array[start.first][start.second].in_use = tid;
            unlock_mutex(start.first, start.second);
            st.push(&cell_array[start.first][start.second]);

            std::mt19937 gen(std::random_device{}());
            std::uniform_real_distribution<> prob(0.0, 1.0);

            while (!st.empty())
            {
                cell* cur = st.top();
                int x = cur->x_cord, y = cur->y_cord;
                unsigned char moves = get_available_neighbors(x, y);

                if (moves == 0)
                {
                    unsigned char visited_dirs = 0;
                    if (isValid(x - 1, y) && cell_array[x - 1][y].in_use && (cur->wall_direction_mask & Up))
                        visited_dirs |= Up;
                    if (isValid(x + 1, y) && cell_array[x + 1][y].in_use && (cur->wall_direction_mask & Down))
                        visited_dirs |= Down;
                    if (isValid(x, y - 1) && cell_array[x][y - 1].in_use && (cur->wall_direction_mask & Left))
                        visited_dirs |= Left;
                    if (isValid(x, y + 1) && cell_array[x][y + 1].in_use && (cur->wall_direction_mask & Right))
                        visited_dirs |= Right;

                    if (visited_dirs && local_loops > 0 && prob(gen) < 0.35)
                    {
                        unsigned char dir = choose_random_move_direction(visited_dirs);
                        lock_mutex(x, y);
                        cur->wall_direction_mask &= ~dir;
                        auto nxt_coords = move({x, y}, dir);
                        lock_mutex(nxt_coords.first, nxt_coords.second);
                        cell* nxt = &cell_array[nxt_coords.first][nxt_coords.second];
                        nxt->wall_direction_mask &= ~get_opposite_direction(dir);
                        nxt->in_use = tid;
                        unlock_mutex(nxt_coords.first, nxt_coords.second);
                        unlock_mutex(x, y);
                        --local_loops;
                    }
                    st.pop();
                }
                else
                {
                    unsigned char dir = choose_random_move_direction(moves);
                    lock_mutex(x, y);
                    cur->wall_direction_mask &= ~dir;
                    auto nxt_coords = move({x, y}, dir);
                    lock_mutex(nxt_coords.first, nxt_coords.second);
                    cell* nxt = &cell_array[nxt_coords.first][nxt_coords.second];
                    nxt->wall_direction_mask &= ~get_opposite_direction(dir);
                    nxt->in_use = tid;
                    st.push(nxt);
                    unlock_mutex(nxt_coords.first, nxt_coords.second);
                    unlock_mutex(x, y);
                }
            }
        });
    }

    for (auto& t : threads)
    {
        if (t.joinable()) t.join();
    }

    auto end_time = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> duration = end_time - start_time;
    std::cout << "Maze generation time: " << std::fixed << std::setprecision(2) << duration.count() << " ms" << std::endl;
}

void MazeSync::generate_multithread_backtrack(std::pair<int, int> start_cell_cords,
                                              std::size_t thread_identifier,
                                              Thread_sync& thread_sync_pointer)
{
    constexpr std::size_t local_sync_interval = 96;
    constexpr std::size_t global_sync_interval = 256;
    constexpr std::size_t local_level = 0;
    constexpr std::size_t global_level = 1;
    std::size_t step_counter = 0;
    thread_sync_pointer.arrive_and_wait(global_level);
    lock_mutex(start_cell_cords.first, start_cell_cords.second);
    std::pair<int, int> current_cell_cords, next_cell_cords;
    std::stack<cell*> cell_stack;
    cell* current_cell;
    cell* next_cell;
    unsigned char current_cell_directions;
    unsigned char move_direction;
    std::size_t next_cell_thread_identifier;
    std::unordered_set<int> locked_mutexes;
    cell_stack.push(&cell_array[start_cell_cords.first][start_cell_cords.second]);
    cell_array[start_cell_cords.first][start_cell_cords.second].in_use = thread_identifier;
    unlock_mutex(start_cell_cords.first, start_cell_cords.second);
    while (!cell_stack.empty())
    {
        current_cell = cell_stack.top();
        current_cell_cords = std::make_pair(current_cell->x_cord, current_cell->y_cord);
        lock_mutex(current_cell_cords.first, current_cell_cords.second);
        locked_mutexes.insert(get_mutex_index(current_cell_cords.first, current_cell_cords.second));
        current_cell_directions = multithread_get_available_neighbors(
            current_cell_cords.first,
            current_cell_cords.second,
            thread_identifier,
            thread_sync_pointer,
            locked_mutexes);
        if (current_cell_directions == 0)
        {
            cell_stack.pop();
            unlock_all_mutexes(locked_mutexes);
            locked_mutexes.clear();
            continue;
        }
        else
        {
            move_direction = choose_random_move_direction(current_cell_directions);
            next_cell_cords = move(current_cell_cords, move_direction);
            next_cell = &cell_array[next_cell_cords.first][next_cell_cords.second];
            next_cell_thread_identifier = next_cell->in_use;
            switch (next_cell_thread_identifier)
            {
                case 0:
                    current_cell->wall_direction_mask &= ~move_direction;
                    next_cell->wall_direction_mask &= ~get_opposite_direction(move_direction);
                    next_cell->in_use = thread_identifier;
                    cell_stack.push(next_cell);
                    unlock_all_mutexes(locked_mutexes);
                    locked_mutexes.clear();
                    break;

                default:
                    thread_sync_pointer.setFlag(thread_identifier);
                    thread_sync_pointer.setFlag(next_cell_thread_identifier);
                    current_cell->wall_direction_mask &= ~move_direction;
                    next_cell->wall_direction_mask &= ~get_opposite_direction(move_direction);
                    cell_stack.push(next_cell);
                    unlock_all_mutexes(locked_mutexes);
                    locked_mutexes.clear();
            }
        }

        ++step_counter;
        if (step_counter % local_sync_interval == 0)
        {
            thread_sync_pointer.arrive_and_wait(local_level);
        }
        if (step_counter % global_sync_interval == 0)
        {
            thread_sync_pointer.arrive_and_wait(global_level);
        }
    }

    thread_sync_pointer.arrive_and_drop(local_level);
    thread_sync_pointer.arrive_and_drop(global_level);
}

} // namespace our
