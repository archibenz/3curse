#!/usr/bin/env bash
# Скрипт для автоматического создания тестового каталога и файлов
# для проверки программы lab3_threads

set -e  # прервать выполнение при ошибке

# === 1. Имя каталога ===
TEST_DIR="./testdir"

# === 2. Очистка старой версии, если была ===
if [[ -d "$TEST_DIR" ]]; then
  echo "Удаляю старый каталог $TEST_DIR..."
  rm -rf "$TEST_DIR"
fi

# === 3. Создание структуры каталогов ===
echo "Создаю структуру каталогов..."
mkdir -p "$TEST_DIR/subdir1"
mkdir -p "$TEST_DIR/subdir2"
mkdir -p "$TEST_DIR/emptydir"

# === 4. Создание файлов разного размера и расширения ===
echo "Создаю файлы..."

# Текстовые файлы
echo "Hello, this is a small text file." > "$TEST_DIR/file1.txt"
echo "Another text file for testing." > "$TEST_DIR/file2.txt"

# Бинарные файлы
head -c 2048 /dev/urandom > "$TEST_DIR/file3.bin"
head -c 10240 /dev/urandom > "$TEST_DIR/file4.bin"

# Лог-файлы
echo "System log start" > "$TEST_DIR/subdir1/file5.log"
dd if=/dev/zero of="$TEST_DIR/subdir2/file6.log" bs=1K count=50 status=none

# Пустой файл
touch "$TEST_DIR/empty.txt"

# === 5. Права доступа (для проверки ошибок) ===
chmod 644 "$TEST_DIR"/*.txt
chmod 600 "$TEST_DIR"/*.bin
chmod 644 "$TEST_DIR"/subdir1/*.log
chmod 644 "$TEST_DIR"/subdir2/*.log

# === 6. Выводим структуру для наглядности ===
echo
echo "✅ Каталог $TEST_DIR успешно создан. Его содержимое:"
echo "-----------------------------------------------------"
ls -lR "$TEST_DIR"
echo "-----------------------------------------------------"
echo "Теперь можешь запустить:"
echo "./lab3_threads $TEST_DIR"
