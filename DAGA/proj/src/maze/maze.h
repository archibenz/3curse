#pragma once

#include <condition_variable>
#include <cstddef>
#include <deque>
#include <mutex>
#include <thread>
#include <unordered_set>
#include <utility>
#include <vector>

namespace our {

enum Directions {
    Up    = 1 << 0,
    Down  = 1 << 1,
    Left  = 1 << 2,
    Right = 1 << 3
};

class Thread_sync {
public:
    std::vector<std::size_t> identifier;
    std::vector<bool> flag;
    std::vector<std::thread>* threads;
    std::mutex mtx;

    explicit Thread_sync(std::vector<std::thread>* t_threads);

    bool get_flag(std::size_t index);
    void setFlag(std::size_t index);
    void arrive_and_wait(std::size_t level);
    void arrive_and_drop(std::size_t level);

    void run();

private:
    struct BarrierState {
        std::mutex mtx;
        std::condition_variable cv;
        std::size_t participants = 0;
        std::size_t arrived = 0;
        std::size_t generation = 0;
    };
    std::deque<BarrierState> barriers;
    void barrier_wait(BarrierState& barrier);
    void barrier_drop(BarrierState& barrier);
};

class cell {
public:
    unsigned char wall_direction_mask;
    int in_use;
    int x_cord;
    int y_cord;

    cell();
    cell(unsigned char C_wall_direction_mask, int C_in_use, int C_x_cord, int C_y_cord);
};

class Maze {
public:
    std::vector<std::vector<cell>> cell_array;
    int length;
    int width;
    std::pair<int, int> start_cell_cords;
    std::pair<int, int> end_cell_cords;

    Maze(int i_length, int i_width);

    int initialization(int i_length, int i_width);
    std::pair<int, int> get_random_start_point();
    std::pair<int, int> get_random_end_point(const std::pair<int, int>& start);
    bool isValid(int x, int y);
    unsigned char get_available_neighbors(int x, int y);
    unsigned char choose_random_move_direction(unsigned char directions);
    std::pair<int, int> move(std::pair<int, int> position, unsigned char direction);
    Directions get_opposite_direction(unsigned char direction);
    int generate_backtrack();
    int generate_imperfect_backtrack(int extra_loops);
};

class MazeSync : public Maze {
public:
    int mutex_cell_size;
    std::vector<std::mutex> mutexes;

    MazeSync(int i_length, int i_width, int i_mutex_cell_size);

    int calculate_mutex_count();
    void initialize_mutexes();
    int get_mutex_index(int x, int y);
    bool try_lock_cell(int x, int y, std::unordered_set<int>& locked_mutexes);
    void lock_mutex(int x, int y);
    void unlock_mutex(int x, int y);
    bool try_lock_mutex_with_timeout(int x, int y, std::unordered_set<int>& locked_mutexes);
    bool is_too_close(const std::pair<int, int>& p1, const std::pair<int, int>& p2);
    std::vector<std::pair<int, int>> generate_random_start_points(int num_points);
    void set_random_start_end_points(std::vector<std::pair<int, int>> points);
    void generate_and_set_random_start_end_points(std::vector<std::pair<int, int>>& extreme_points);
    unsigned char check_another_thread_identifier(std::size_t thread_identifier,
                                                   std::size_t another_thread_identifier,
                                                   Thread_sync& thread_sync_pointer,
                                                   Directions direction);
    unsigned char multithread_get_available_neighbors(int x,
                                                      int y,
                                                      std::size_t thread_identifier,
                                                      Thread_sync& thread_sync_pointer,
                                                      std::unordered_set<int>& locked_mutexes);
    void unlock_all_mutexes(std::unordered_set<int>& locked_mutexes);
    void generate_multithread_imperfect(int extra_loops, int num_threads);
    void generate_multithread_backtrack(std::pair<int, int> start_cell_cords,
                                        std::size_t thread_identifier,
                                        Thread_sync& thread_sync_pointer);
};

} // namespace our
