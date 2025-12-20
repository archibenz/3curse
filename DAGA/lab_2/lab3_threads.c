#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>

#define MAX_FILES 1024
#define MAX_NAME 512

// Глобальные переменные
char files[MAX_FILES][MAX_NAME];        // очередь файлов для обработки
int file_count = 0;
int read_index = 0;
int search_done = 0;

char unique_files[MAX_FILES][MAX_NAME]; // уникальные файлы для итогового отчета
int unique_count = 0;

pthread_mutex_t mutex;
pthread_cond_t cond;
FILE* log_file;

// ================= ВСПОМОГАТЕЛЬНАЯ ФУНКЦИЯ =================
void random_delay() {
    usleep(100000 + rand() % 200000);
}

// Проверка на уникальность для итогового отчета
int unique_file_exists(const char* path) {
    for (int i = 0; i < unique_count; i++)
        if (strcmp(unique_files[i], path) == 0)
            return 1;
    return 0;
}

// ================= ПОТОК 1 — ПОИСК =================
void* search_files(void* arg) {
    const char* dirpath = (const char*)arg;
    int iterations = 20 + rand() % 11;

    for (int i = 0; i < iterations; i++) {
        pthread_mutex_lock(&mutex);
        fprintf(log_file, "[ПОТОК 1] Ищет файлы (итерация %d)\n", i + 1);
        fflush(log_file);
        pthread_mutex_unlock(&mutex);

        DIR* dir = opendir(dirpath);
        if (!dir) { perror("Ошибка открытия каталога"); pthread_exit(NULL); }

        struct dirent* entry;
        while ((entry = readdir(dir)) != NULL) {
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
                continue;

            char path[MAX_NAME];
            snprintf(path, sizeof(path), "%s/%s", dirpath, entry->d_name);

            struct stat st;
            if (stat(path, &st) == 0 && S_ISREG(st.st_mode)) {
                pthread_mutex_lock(&mutex);
                if (file_count < MAX_FILES) {
                    strncpy(files[file_count], path, MAX_NAME);
                    file_count++;

                    // логируем все действия
                    fprintf(log_file, "[ПОТОК 1] Найден файл: %s (%ld байт)\n", path, st.st_size);
                    fflush(log_file);

                    // добавляем в уникальные файлы для итогового отчета, если ещё нет
                    if (!unique_file_exists(path) && unique_count < MAX_FILES) {
                        strncpy(unique_files[unique_count], path, MAX_NAME);
                        unique_count++;
                    }

                    pthread_cond_signal(&cond);
                }
                pthread_mutex_unlock(&mutex);
            }
        }
        closedir(dir);
        random_delay();
    }

    pthread_mutex_lock(&mutex);
    search_done = 1;
    pthread_cond_signal(&cond);
    fprintf(log_file, "[ПОТОК 1] Завершил поиск файлов.\n");
    fflush(log_file);
    pthread_mutex_unlock(&mutex);

    pthread_exit(NULL);
}

// ================= ПОТОК 2 — ОБРАБОТКА =================
void* process_files(void* arg) {
    (void)arg;
    long long total_size = 0;
    int iterations = 20 + rand() % 11;

    for (int i = 0; i < iterations; i++) {
        pthread_mutex_lock(&mutex);
        while (read_index >= file_count && !search_done)
            pthread_cond_wait(&cond, &mutex);

        if (read_index < file_count) {
            char path[MAX_NAME];
            strncpy(path, files[read_index], MAX_NAME);
            read_index++;
            pthread_mutex_unlock(&mutex);

            struct stat st;
            if (stat(path, &st) == 0) {
                total_size += st.st_size;
                pthread_mutex_lock(&mutex);
                fprintf(log_file, "[ПОТОК 2] Обрабатывает: %-40s | Размер: %10ld байт | Всего: %lld байт\n",
                        path, st.st_size, total_size);
                fflush(log_file);
                pthread_mutex_unlock(&mutex);
            }
        } else {
            pthread_mutex_unlock(&mutex);
            if (search_done) break;
        }
        random_delay();
    }

    pthread_mutex_lock(&mutex);
    fprintf(log_file, "[ПОТОК 2] Завершил обработку. Общий размер: %lld байт\n", total_size);
    fflush(log_file);
    pthread_mutex_unlock(&mutex);

    pthread_exit(NULL);
}

// ================= MAIN =================
int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("Использование: %s <каталог>\n", argv[0]);
        return 1;
    }

    srand(time(NULL));

    pthread_mutex_init(&mutex, NULL);
    pthread_cond_init(&cond, NULL);

    log_file = fopen("detailed_log.txt", "w");
    if (!log_file) { perror("Ошибка создания detailed_log.txt"); return 1; }

    pthread_t producer, consumer;
    pthread_create(&producer, NULL, search_files, argv[1]);
    pthread_create(&consumer, NULL, process_files, NULL);

    pthread_join(producer, NULL);
    pthread_join(consumer, NULL);

    fclose(log_file);
    pthread_mutex_destroy(&mutex);
    pthread_cond_destroy(&cond);

    // === итоговый отчёт без повторов ===
    FILE* report = fopen("final_report.txt", "w");
    if (!report) { perror("Ошибка создания final_report.txt"); return 1; }

    fprintf(report, "=== Итоговый отчёт по уникальным файлам ===\n\n");
    long long total = 0;
    for (int i = 0; i < unique_count; i++) {
        struct stat st;
        if (stat(unique_files[i], &st) == 0) {
            fprintf(report, "%-50s %10ld байт\n", unique_files[i], st.st_size);
            total += st.st_size;
        }
    }
    fprintf(report, "\nВсего уникальных файлов: %d\nОбщий размер: %lld байт\n", unique_count, total);
    fclose(report);

    printf("\n✅ Работа завершена.\n");
    printf("📄 Подробный лог: detailed_log.txt\n");
    printf("📄 Итоговый отчёт: final_report.txt (только уникальные файлы)\n");

    return 0;
}




