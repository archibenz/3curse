#include <windows.h>
#include <vector>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <random>
#include <iostream>

// Размер поля
const int BOARD_SIZE = 5;
const int MAX_STALE_MOVES = 100; // Максимальное количество ходов без изменений

// Игровое поле
std::vector<std::vector<char>> board(BOARD_SIZE, std::vector<char>(BOARD_SIZE, '.'));

// Мьютекс для синхронизации потоков
std::mutex mtx;
std::condition_variable cv;
bool player1_turn = true;
bool game_over = false;

// Генератор случайных чисел
std::random_device rd;
std::mt19937 gen(rd());
std::uniform_int_distribution<> dist(0, BOARD_SIZE - 1);

// Счётчик для ходов без изменений
int stale_move_counter = 0;

// Инициализация начального положения шашек
void initializeBoard() {
    for (int i = 0; i < BOARD_SIZE; ++i) {
        for (int j = 0; j < BOARD_SIZE; ++j) {
            if (i < 2 && (i + j) % 2 == 1) {
                board[i][j] = 'x'; // Игрок 1
            }
            else if (i >= BOARD_SIZE - 2 && (i + j) % 2 == 1) {
                board[i][j] = 'o'; // Игрок 2
            }
        }
    }
}

// Проверка состояния игры
bool checkGameOver() {
    int player1_pieces = 0;
    int player2_pieces = 0;

    for (int i = 0; i < BOARD_SIZE; ++i) {
        for (int j = 0; j < BOARD_SIZE; ++j) {
            if (board[i][j] == 'x') player1_pieces++;
            if (board[i][j] == 'o') player2_pieces++;
        }
    }

    if (player1_pieces == 0 || player2_pieces == 0) {
        return true;
    }

    // Проверка на ничью
    if (stale_move_counter >= MAX_STALE_MOVES) {
        MessageBox(NULL, L"Ничья: слишком много ходов без изменений!", L"Игра окончена", MB_OK);
        return true;
    }

    return false;
}

// Функция для случайного хода
bool makeRandomMove(char player_piece) {
    std::vector<std::pair<int, int>> possible_moves;

    // Находим все возможные ходы для текущего игрока
    for (int x = 0; x < BOARD_SIZE; ++x) {
        for (int y = 0; y < BOARD_SIZE; ++y) {
            if (board[x][y] == player_piece) {
                int direction = (player_piece == 'x') ? 1 : -1; // Направление движения
                int new_x = x + direction;

                // Проверка на возможность хода вправо
                if (new_x >= 0 && new_x < BOARD_SIZE && y + 1 < BOARD_SIZE && board[new_x][y + 1] == '.') {
                    possible_moves.emplace_back(x, y);
                }
                // Проверка на возможность хода влево
                if (new_x >= 0 && new_x < BOARD_SIZE && y - 1 >= 0 && board[new_x][y - 1] == '.') {
                    possible_moves.emplace_back(x, y);
                }
            }
        }
    }

    // Если нет возможных ходов
    if (possible_moves.empty()) {
        return false;
    }

    // Выбираем случайный ход из доступных
    std::uniform_int_distribution<> move_dist(0, possible_moves.size() - 1);
    int move_index = move_dist(gen);
    int x = possible_moves[move_index].first;
    int y = possible_moves[move_index].second;

    int direction = (player_piece == 'x') ? 1 : -1;
    int new_y = (dist(gen) % 2 == 0) ? y + 1 : y - 1;
    int new_x = x + direction;

    // Выполняем ход
    if (new_x >= 0 && new_x < BOARD_SIZE && new_y >= 0 && new_y < BOARD_SIZE && board[new_x][new_y] == '.') {
        board[x][y] = '.';
        board[new_x][new_y] = player_piece;
        return true;
    }

    return false;
}

// Ход игрока 1
void player1() {
    while (!game_over) {
        std::unique_lock<std::mutex> lock(mtx);
        cv.wait(lock, [] { return player1_turn; });

        if (game_over) break;

        if (makeRandomMove('x')) {
            stale_move_counter = 0; // Обнуляем счётчик ходов без изменений
        }
        else {
            stale_move_counter++;
        }

        game_over = checkGameOver();
        player1_turn = false;
        lock.unlock();
        cv.notify_all();
    }
}

// Ход игрока 2
void player2() {
    while (!game_over) {
        std::unique_lock<std::mutex> lock(mtx);
        cv.wait(lock, [] { return !player1_turn; });

        if (game_over) break;


            if (makeRandomMove('o')) {
                stale_move_counter = 0; // Обнуляем счётчик ходов без изменений
            }
            else {
                stale_move_counter++;
            }

        game_over = checkGameOver();
        player1_turn = true;
        lock.unlock();
        cv.notify_all();
    }
}

// Отрисовка игрового поля
void DrawBoard(HDC hdc) {
    RECT rect;
    GetClientRect(GetActiveWindow(), &rect);
    int cellSize = (rect.right - rect.left) / BOARD_SIZE;

    for (int i = 0; i < BOARD_SIZE; ++i) {
        for (int j = 0; j < BOARD_SIZE; ++j) {
            int x = j * cellSize;
            int y = i * cellSize;

            // Рисуем клетку
            Rectangle(hdc, x, y, x + cellSize, y + cellSize);

            // Рисуем шашку
            if (board[i][j] == 'x') {
                // Рисуем кружок для игрока 1
                Ellipse(hdc, x + 5, y + 5, x + cellSize - 5, y + cellSize - 5);
            }
            else if (board[i][j] == 'o') {
                // Рисуем крестик для игрока 2
                MoveToEx(hdc, x + 5, y + 5, NULL);
                LineTo(hdc, x + cellSize - 5, y + cellSize - 5);
                MoveToEx(hdc, x + cellSize - 5, y + 5, NULL);
                LineTo(hdc, x + 5, y + cellSize - 5);
            }
        }
    }
}

// Обработчик сообщений окна
LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    switch (uMsg) {
    case WM_PAINT: {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint(hwnd, &ps);
        DrawBoard(hdc);
        EndPaint(hwnd, &ps);
        break;
    }
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    default:
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    return 0;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    const wchar_t CLASS_NAME[] = L"Шашки";

    WNDCLASS wc = {};
    wc.lpfnWndProc = WindowProc;
    wc.hInstance = hInstance;
    wc.lpszClassName = CLASS_NAME;

    RegisterClass(&wc);

    // Устанавливаем размер окна, чтобы он соответствовал размеру игрового поля
    int windowWidth = BOARD_SIZE * 80;
    int windowHeight = BOARD_SIZE * 80;

    HWND hwnd = CreateWindowEx(
        0,
        CLASS_NAME,
        L"Шашки",
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT, CW_USEDEFAULT, windowWidth, windowHeight,
        NULL,
        NULL,
        hInstance,
        NULL
    );

    if (hwnd == NULL) {
        return 0;
    }

    ShowWindow(hwnd, nCmdShow);

    initializeBoard();

    std::thread t1(player1);
    std::thread t2(player2);

    MSG msg = {};
    while (GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    t1.join();
    t2.join();

    return 0;
}