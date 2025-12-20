#pragma once

#include "maze.h"

#include <utility>
#include <vector>

namespace our {

std::vector<std::pair<int, int>> add_border_exits(MazeSync& maze, int n);

void test_multithread_generation_time(int length,
                                      int width,
                                      int test_variable_mutex_cell_size_min,
                                      int test_variable_mutex_cell_size_max,
                                      int num_tests,
                                      int num_threads);

void test_generation_time_by_thread_num(int length,
                                        int width,
                                        int min_num_threads,
                                        int max_num_threads,
                                        int num_tests,
                                        int test_mutex_cell_size,
                                        bool want_one_thread);
void test_generation_time_comparison(int length,
                                     int width,
                                     int min_num_threads,
                                     int max_num_threads,
                                     int num_tests,
                                     int test_mutex_cell_size);

int get_optimal_thread_num(int length, int width, int mutex_cell_size);

void printMathDescriptions();
void calcArraySize();
void test();

} // namespace our
