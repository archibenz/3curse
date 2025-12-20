#include "maze_generation.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <fstream>
#include <future>
#include <iomanip>
#include <iostream>
#include <random>
#include <sstream>
#include <stack>
#include <thread>

namespace our {

unsigned char Maze::get_available_neighbors(int x, int y)
{
    unsigned char available_directions = 0;

    if (isValid(x - 1, y) && !cell_array[x - 1][y].in_use)
    {
        available_directions |= Up;
    }

    if (isValid(x + 1, y) && !cell_array[x + 1][y].in_use)
    {
        available_directions |= Down;
    }

    if (isValid(x, y - 1) && !cell_array[x][y - 1].in_use)
    {
        available_directions |= Left;
    }

    if (isValid(x, y + 1) && !cell_array[x][y + 1].in_use)
    {
        available_directions |= Right;
    }

    return available_directions;
}

unsigned char Maze::choose_random_move_direction(unsigned char directions)
{
    if (directions == 0)
    {
        return 0;
    }
    std::vector<Directions> available_directions;
    if (directions & Up)
    {
        available_directions.push_back(Up);
    }
    if (directions & Down)
    {
        available_directions.push_back(Down);
    }
    if (directions & Left)
    {
        available_directions.push_back(Left);
    }
    if (directions & Right)
    {
        available_directions.push_back(Right);
    }
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dist(0, static_cast<int>(available_directions.size()) - 1);

    return available_directions[dist(gen)];
}

std::pair<int, int> Maze::move(std::pair<int, int> position, unsigned char direction)
{
    int x = position.first;
    int y = position.second;
    switch (direction)
    {
        case Up:
            return {x - 1, y};
        case Down:
            return {x + 1, y};
        case Left:
            return {x, y - 1};
        case Right:
            return {x, y + 1};
        default:
            throw std::invalid_argument("Недопустимое направление движения.");
    }
}

Directions Maze::get_opposite_direction(unsigned char direction)
{
    switch (static_cast<Directions>(direction))
    {
        case Up: return Down;
        case Down: return Up;
        case Left: return Right;
        case Right: return Left;
        default: throw std::invalid_argument("Недопустимое направление движения.");
    }
}

int Maze::generate_backtrack()
{
    std::pair<int, int> current_cell_cords, next_cell_cords;
    cell* current_cell;
    cell* next_cell;
    unsigned char current_cell_directions;
    unsigned char move_direction;
    std::stack<cell*> cell_stack;

    cell_stack.push(&cell_array[start_cell_cords.first][start_cell_cords.second]);
    cell_array[start_cell_cords.first][start_cell_cords.second].in_use = 1;

    while (!cell_stack.empty())
    {
        current_cell = cell_stack.top();
        current_cell_cords = std::make_pair(current_cell->x_cord, current_cell->y_cord);
        current_cell_directions = get_available_neighbors(current_cell_cords.first, current_cell_cords.second);

        if (current_cell_directions == 0)
        {
            cell_stack.pop();
            continue;
        }
        else
        {
            move_direction = choose_random_move_direction(current_cell_directions);
            current_cell->wall_direction_mask &= ~move_direction;

            next_cell_cords = move(current_cell_cords, move_direction);
            next_cell = &cell_array[next_cell_cords.first][next_cell_cords.second];

            next_cell->wall_direction_mask &= ~get_opposite_direction(move_direction);
            next_cell->in_use = 1;

            cell_stack.push(next_cell);
        }
    }

    return 1;
}

int Maze::generate_imperfect_backtrack(int extra_loops)
{
    std::pair<int, int> cur, nxt;
    cell *curCell, *nxtCell;
    unsigned char moves, dir;
    std::stack<cell*> st;

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> prob(0.0, 1.0);

    st.push(&cell_array[start_cell_cords.first][start_cell_cords.second]);
    cell_array[start_cell_cords.first][start_cell_cords.second].in_use = 1;

    while (!st.empty())
    {
        curCell = st.top();
        cur = {curCell->x_cord, curCell->y_cord};
        moves = get_available_neighbors(cur.first, cur.second);

        if (moves == 0)
        {
            if (extra_loops > 0)
            {
                unsigned char visited = 0;
                int x = cur.first, y = cur.second;
                if (isValid(x - 1, y) && cell_array[x - 1][y].in_use && (curCell->wall_direction_mask & Up)) visited |= Up;
                if (isValid(x + 1, y) && cell_array[x + 1][y].in_use && (curCell->wall_direction_mask & Down)) visited |= Down;
                if (isValid(x, y - 1) && cell_array[x][y - 1].in_use && (curCell->wall_direction_mask & Left)) visited |= Left;
                if (isValid(x, y + 1) && cell_array[x][y + 1].in_use && (curCell->wall_direction_mask & Right)) visited |= Right;

                if (visited && prob(gen) < 0.35)
                {
                    dir = choose_random_move_direction(visited);
                    curCell->wall_direction_mask &= ~dir;
                    nxt = move(cur, dir);
                    nxtCell = &cell_array[nxt.first][nxt.second];
                    nxtCell->wall_direction_mask &= ~get_opposite_direction(dir);
                    --extra_loops;
                }
            }
            st.pop();
            continue;
        }

        dir = choose_random_move_direction(moves);
        curCell->wall_direction_mask &= ~dir;
        nxt = move(cur, dir);
        nxtCell = &cell_array[nxt.first][nxt.second];
        nxtCell->wall_direction_mask &= ~get_opposite_direction(dir);
        nxtCell->in_use = 1;
        st.push(nxtCell);
    }
    return 1;
}

void test()
{
    std::cout << "YES, I LOVE MY..." << std::endl;
}

std::vector<std::pair<int,int>> add_border_exits(MazeSync& maze, int n)
{
    std::vector<std::pair<int,int>> border;
    for(int x=0; x<maze.length; ++x){
        border.push_back({x,0});
        border.push_back({x,maze.width-1});
    }
    for(int y=1; y<maze.width-1; ++y){
        border.push_back({0,y});
        border.push_back({maze.length-1,y});
    }
    border.erase(std::remove(border.begin(),border.end(),maze.start_cell_cords), border.end());

    std::random_device rd; std::mt19937 gen(rd());
    std::shuffle(border.begin(), border.end(), gen);
    if(n > (int)border.size()) n = border.size();

    std::vector<std::pair<int,int>> exits;
    exits.reserve(n);

    auto carve = [&](int x,int y){
        if(x==0)        maze.cell_array[x][y].wall_direction_mask &= ~Up;
        else if(x==maze.length-1) maze.cell_array[x][y].wall_direction_mask &= ~Down;
        else if(y==0)   maze.cell_array[x][y].wall_direction_mask &= ~Left;
        else            maze.cell_array[x][y].wall_direction_mask &= ~Right;
    };

    for(int i=0;i<n;++i){
        auto c = border[i];
        carve(c.first,c.second);
        exits.push_back(c);
    }
    return exits;
}

void test_multithread_generation_time(
    int length,
    int width,
    int test_variable_mutex_cell_size_min,
    int test_variable_mutex_cell_size_max,
    int num_tests,
    int num_threads
)
{
    std::ostringstream filename_stream;
    filename_stream << "test_multithread_generation_times_" << num_threads << "threads.csv";
    std::ofstream csv_file(filename_stream.str());
    if (!csv_file.is_open())
    {
        throw std::invalid_argument("Ошибка с открытием файла.");
        return;
    }
    csv_file << "№" << "," << "размер мьютексной секции" <<","<< "время" <<"\n";
    for (
        int test_mutex_cell_size = test_variable_mutex_cell_size_min;
        test_mutex_cell_size <= test_variable_mutex_cell_size_max;
        ++test_mutex_cell_size
        )
    {
        for (int test = 1; test <= num_tests; ++test)
        {
            MazeSync my_sync(length, width, test_mutex_cell_size);
            std::vector<std::thread> threads(num_threads);
            Thread_sync myStruct(&threads);
            std::vector<std::pair<int, int>> start_points = my_sync.generate_random_start_points(num_threads);
            my_sync.set_random_start_end_points(start_points);

            auto start_time = std::chrono::high_resolution_clock::now();

            for (int i = 0; i < num_threads; ++i)
            {
                threads[i] = std::thread(&MazeSync::generate_multithread_backtrack,
                                         &my_sync, start_points[i], i + 1, std::ref(myStruct));
            }
            for (auto& t : threads) {
                if (t.joinable())
                {
                    t.join();
                }
            }
            start_points.clear();

            auto end_time = std::chrono::high_resolution_clock::now();
            std::chrono::duration<double, std::milli> duration = end_time - start_time;
            csv_file << test << "," << test_mutex_cell_size<<"," <<std::fixed << std::setprecision(2) << duration.count() << "\n";
        }
    }

    csv_file.close();
    std::cout <<filename_stream.str()<< std::endl;
}

void test_generation_time_by_thread_num(
    int length,
    int width,
    int min_num_threads,
    int max_num_threads,
    int num_tests,
    int test_mutex_cell_size,
    bool want_one_thread)
{
    std::ostringstream filename_stream;
    filename_stream << "test_generation_time_by_thread_num_" << test_mutex_cell_size << "mutex_cell_size.csv";
    std::ofstream csv_file(filename_stream.str());
    if (!csv_file.is_open())
    {
        throw std::invalid_argument("Ошибка с открытием файла.");
        return;
    }

    csv_file << "№" << "," << "Количество потоков" <<","<< "время" <<"\n";
    csv_file.flush();
    std::cerr << "[INFO] CSV header written, path = " << filename_stream.str() << std::endl;
    if (want_one_thread)
    {
        for (int test = 1; test <= num_tests; ++test)
        {
            Maze my(length, width);
            auto start_time = std::chrono::high_resolution_clock::now();
            my.generate_backtrack();
            auto end_time = std::chrono::high_resolution_clock::now();
            std::chrono::duration<double, std::milli> duration = end_time - start_time;
            csv_file << test << "," << "1"<<"," <<std::fixed << std::setprecision(2) << duration.count() << "\n";
        }
    }

    for (
        int num_threads = min_num_threads;
        num_threads <= max_num_threads;
        ++num_threads
    )
    {
        std::vector<std::future<double>> fut;
        fut.reserve(num_tests);

        for (int test = 1; test <= num_tests; ++test)
        {
            fut.emplace_back(std::async(std::launch::async, [=]() -> double {
                MazeSync my_sync(length, width, test_mutex_cell_size);
                std::vector<std::thread> threads(num_threads);
                std::vector<std::pair<int, int>> start_points;
                Thread_sync myStruct(&threads);
                my_sync.generate_and_set_random_start_end_points(start_points);

                auto t0 = std::chrono::high_resolution_clock::now();
                for (int i = 0; i < num_threads; ++i) {
                    threads[i] = std::thread(&MazeSync::generate_multithread_backtrack,
                                             &my_sync, start_points[i], i + 1, std::ref(myStruct));
                }
                for (auto& t : threads) if (t.joinable()) t.join();

                auto dt = std::chrono::high_resolution_clock::now() - t0;
                return std::chrono::duration<double,std::milli>(dt).count();
            }));
        }

        for (int test = 1; test <= num_tests; ++test)
        {
            double ms = fut[test-1].get();
            csv_file << test << "," << num_threads << ","
                     << std::fixed << std::setprecision(2) << ms << "\n";
        }
    }

    csv_file.close();
    std::cout <<filename_stream.str()<< std::endl;
}

int get_optimal_thread_num(int length, int width, int mutex_cell_size)
{
    int optimal_thread_num = 2 *( std::ceil(static_cast<double>(length)/mutex_cell_size) + std::ceil(static_cast<double>(width)/mutex_cell_size)- 2);
    return optimal_thread_num;
}

void printMathDescriptions()
{
    std::cout <<
    "1) Classic backtracking:\n"
    "2) Non-perfect maze (optimised backtracking):\n"
    "   Modified DFS knocks down a wall to an already visited neighbour\n"
    "   while generating, until exactly n additional passages appear.\n"
    "   Time: O(W*H), Memory: O(W*H)\n\n"
    "3) Simple 20x20:\n"
    "   Random border openings without DFS.\n"
    "   Time: O(1)\n";
}

void calcArraySize()
{
    int w,h;
    std::cout << "Введите ширину и высоту: ";
    if(!(std::cin>>w>>h)) return;
    long cells = static_cast<long>(w) * h;
    long bytes = cells * sizeof(our::cell) * 100;
    std::cout << "Для 100 лабиринтов потребуется " << bytes << " байт\n";
    std::cout << "Неидеальный алгоритм использует ту же память — структура клеток не меняется.\n";
}

} // namespace our
