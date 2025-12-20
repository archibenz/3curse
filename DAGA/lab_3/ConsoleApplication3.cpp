#include <windows.h>
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <chrono>
#include <iomanip>
#include <sstream>

class Philosopher {
private:
    int id;
    HANDLE leftFork;
    HANDLE rightFork;
    int mealsEaten;

public:
    Philosopher(int id, HANDLE left, HANDLE right)
        : id(id), leftFork(left), rightFork(right), mealsEaten(0) {
    }

    void dine(bool& running) {
        while (running) {
            think();
            eat();
        }
    }

    void think() {
        std::string message = "Философ " + std::to_string(id) + " размышляет...";
        std::cout << message << std::endl;
        logToFile(message);
        Sleep(rand() % 2000 + 1000);
    }

    void eat() {
        DWORD leftResult = WaitForSingleObject(leftFork, 1500);

        if (leftResult == WAIT_OBJECT_0) {
            std::string message = "Философ " + std::to_string(id) + " взял левую вилку";
            std::cout << message << std::endl;
            logToFile(message);

            DWORD rightResult = WaitForSingleObject(rightFork, 3000);

            if (rightResult == WAIT_OBJECT_0) {
                std::string message = "Философ " + std::to_string(id) + " взял правую вилку и начал есть";
                std::cout << message << std::endl;
                logToFile(message);

                mealsEaten++;
                Sleep(rand() % 2000 + 1000);

                message = "Философ " + std::to_string(id) + " поел (" + std::to_string(mealsEaten) + " раз) и положил вилки";
                std::cout << message << std::endl;
                logToFile(message);

                ReleaseMutex(rightFork);
            }
            else {
                std::string message = "Философ " + std::to_string(id) + " не смог взять правую вилку и продолжает думать";
                std::cout << message << std::endl;
                logToFile(message);
            }

            ReleaseMutex(leftFork);

            if (rightResult == WAIT_OBJECT_0) {
                Sleep(500);
            }
        }
    }

    int getMealsEaten() const {
        return mealsEaten;
    }

private:
    void logToFile(const std::string& message) {
        auto now = std::chrono::system_clock::now();
        auto time_t = std::chrono::system_clock::to_time_t(now);
        auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()) % 1000;

        std::tm tm;
        localtime_s(&tm, &time_t);

        std::ostringstream timestamp;
        timestamp << std::put_time(&tm, "%Y-%m-%d %H:%M:%S");
        timestamp << "." << std::setfill('0') << std::setw(3) << ms.count();

        std::ofstream file("philosophers_log.txt", std::ios::app);
        if (file.is_open()) {
            file << "[" << timestamp.str() << "] " << message << std::endl;
            file.close();
        }
    }
};

std::vector<HANDLE> forks;
std::vector<Philosopher*> philosophers;
std::vector<HANDLE> threads;
bool running = true;

DWORD WINAPI philosopherThread(LPVOID param) {
    int philosopherId = (int)(intptr_t)param;
    philosophers[philosopherId]->dine(running);
    return 0;
}

void logToFile(const std::string& message) {
    auto now = std::chrono::system_clock::now();
    auto time_t = std::chrono::system_clock::to_time_t(now);
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()) % 1000;

    std::tm tm;
    localtime_s(&tm, &time_t);

    std::ostringstream timestamp;
    timestamp << std::put_time(&tm, "%Y-%m-%d %H:%M:%S");
    timestamp << "." << std::setfill('0') << std::setw(3) << ms.count();

    std::ofstream file("philosophers_log.txt", std::ios::app);
    if (file.is_open()) {
        file << "[" << timestamp.str() << "] " << message << std::endl;
        file.close();
    }
}

void printStatus() {
    std::cout << "\n=== Текущее состояние ===" << std::endl;
    logToFile("=== Текущее состояние ===");

for (size_t i = 0; i < philosophers.size(); i++) {
    std::string status = "Философ " + std::to_string(i) + ": поел " +
        std::to_string(philosophers[i]->getMealsEaten()) + " раз";
    std::cout << status << std::endl;
    logToFile(status);
}

std::cout << "========================\n" << std::endl;
logToFile("========================");
}

int main() {
    setlocale(0, "Rus");
    srand(GetTickCount());

    const int NUM_PHILOSOPHERS = 5;

    // Очистка файла при запуске
    std::ofstream clearFile("philosophers_log.txt");
    clearFile.close();

    std::string startupMessage = "Программа запущена. Создано " +
        std::to_string(NUM_PHILOSOPHERS) + " философов и " +
        std::to_string(NUM_PHILOSOPHERS) + " вилок";
    std::cout << startupMessage << std::endl;
    logToFile(startupMessage);

    std::cout << "Команды:" << std::endl;
    std::cout << "  Enter - показать статус" << std::endl;
    std::cout << "  q + Enter - выход" << std::endl;
    std::cout << "  a + Enter - добавить философа" << std::endl;

    for (int i = 0; i < NUM_PHILOSOPHERS; i++) {
        HANDLE fork = CreateMutex(NULL, FALSE, NULL);
        forks.push_back(fork);
    }

    for (int i = 0; i < NUM_PHILOSOPHERS; i++) {
        HANDLE leftFork = forks[i];
        HANDLE rightFork = forks[(i + 1) % NUM_PHILOSOPHERS];

        philosophers.push_back(new Philosopher(i, leftFork, rightFork));
    }

    for (int i = 0; i < NUM_PHILOSOPHERS; i++) {
        HANDLE thread = CreateThread(NULL, 0, philosopherThread, (LPVOID)(intptr_t)i, 0, NULL);
        threads.push_back(thread);
    }

    std::string input;
    int philosopherCount = NUM_PHILOSOPHERS;

    while (running) {
        std::getline(std::cin, input);

        if (input == "q"  input == "Q") {
            running = false;
            std::string stopMessage = "Завершение работы...";
            std::cout << stopMessage << std::endl;
            logToFile(stopMessage);
        }
        else if (input == "") {
            printStatus();
        }
        else if (input == "a"  input == "A") {
            HANDLE newFork = CreateMutex(NULL, FALSE, NULL);
            forks.push_back(newFork);

            HANDLE leftFork = forks[philosopherCount];
            HANDLE rightFork = forks[0];

            philosophers.push_back(new Philosopher(philosopherCount, leftFork, rightFork));

            HANDLE thread = CreateThread(NULL, 0, philosopherThread, (LPVOID)(intptr_t)philosopherCount, 0, NULL);
            threads.push_back(thread);

            std::string addMessage = "Добавлен философ #" + std::to_string(philosopherCount);
            std::cout << addMessage << std::endl;
            logToFile(addMessage);
            philosopherCount++;
        }
    }

    WaitForMultipleObjects((DWORD)threads.size(), threads.data(), TRUE, INFINITE);

    for (HANDLE thread : threads) {
        CloseHandle(thread);
    }

    for (HANDLE fork : forks) {
        CloseHandle(fork);
    }

    for (Philosopher* philosopher : philosophers) {
        delete philosopher;
    }

    std::cout << "\n=== Финальная статистика ===" << std::endl;
    logToFile("=== Финальная статистика ===");

    for (size_t i = 0; i < philosophers.size(); i++) {
        std::string finalStatus = "Философ " + std::to_string(i) + " всего поел: " +
            std::to_string(philosophers[i]->getMealsEaten()) + " раз";
        std::cout << finalStatus << std::endl;
        logToFile(finalStatus);
    }

    std::string endMessage = "Программа завершена";
    std::cout << endMessage << std::endl;
    logToFile(endMessage);

    return 0;
}