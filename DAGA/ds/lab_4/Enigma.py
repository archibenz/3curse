import threading
from concurrent.futures import ThreadPoolExecutor
from threading import Lock
import time

class EnigmaMachine:
    def __init__(self, rotors, reflector):
        """
        Инициализация шифра Энигмы.
        :param rotors: Список роторов (каждый ротор - строка символов).
        :param reflector: Отражатель (строка символов).
        """
        self.rotors = rotors
        self.reflector = reflector
        self.alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    def encrypt_char(self, char):
        """
        Шифрование одного символа.
        :param char: Символ для шифрования.
        :return: Зашифрованный символ.
        """
        if char not in self.alphabet:
            return char  # Не шифруем символы, не входящие в алфавит

        # Проход через роторы
        for rotor in self.rotors:
            char = rotor[self.alphabet.index(char)]

        # Проход через отражатель
        char = self.reflector[self.alphabet.index(char)]

        # Обратный проход через роторы
        for rotor in reversed(self.rotors):
            char = self.alphabet[rotor.index(char)]

        # Сдвиг роторов
        self._rotate_rotors()

        return char

    def _rotate_rotors(self):
        """Сдвиг роторов."""
        for i in range(len(self.rotors)):
            self.rotors[i] = self.rotors[i][1:] + self.rotors[i][0]

    def encrypt_text(self, text):
        """
        Шифрование текста.
        :param text: Текст для шифрования.
        :return: Зашифрованный текст.
        """
        return ''.join(self.encrypt_char(char.upper()) for char in text)


def encrypt_part(part, enigma, lock, file, log_lock, log_file):
    """
    Шифрование части текста.
    :param part: Часть текста.
    :param enigma: Экземпляр шифра Энигмы.
    :param lock: Мьютекс для синхронизации записи в файл.
    :param file: Файл для записи результата.
    :param log_lock: Мьютекс для синхронизации записи в лог-файл.
    :param log_file: Лог-файл для записи действий потоков.
    :return: Зашифрованная часть текста.
    """
    # Логирование взятия задачи
    with log_lock:
        log_file.write(f"[THREAD {threading.get_ident()}] Took task: {part}\n")

    encrypted_part = enigma.encrypt_text(part)

    # Логирование завершения задачи
    with log_lock:
        log_file.write(f"[THREAD {threading.get_ident()}] Completed task: {part}\n")

    # Блокировка для синхронизированной записи в файл
    with lock:
        file.write(encrypted_part + "\n")

    return encrypted_part


def main():
    # Пример роторов и отражателя
    rotors = [
        "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
        "AJDKSIRUXBLHWTMCQGZNPYFVOE",
        "BDFHJLCPRTXVZNYEIWGAKMUSQO"
    ]
    reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

    # Создание экземпляра шифра Энигмы
    enigma = EnigmaMachine(rotors, reflector)

    # Ввод текста пользователем
    text = input("Введите текст для шифрования: ")

    # Разделение текста на части
    num_threads = 4
    part_size = len(text) // num_threads
    parts = [text[i * part_size:(i + 1) * part_size] for i in range(num_threads)]
    if len(text) % num_threads != 0:  # Добавляем оставшуюся часть, если текст не делится нацело
        parts.append(text[num_threads * part_size:])

    # Мьютексы для синхронизации записи в файл и лог-файл
    lock = Lock()
    log_lock = Lock()

    # Открытие файлов для записи результата и логирования
    with open("encrypted_text.txt", "w") as file, open("mutex_log.txt", "w") as log_file:
        log_file.write("Logging started...\n")

        # Использование ThreadPoolExecutor для многопоточной обработки
        with ThreadPoolExecutor(max_workers=num_threads) as executor:
            # Шифрование каждой части текста в отдельном потоке
            encrypted_parts = list(executor.map(encrypt_part, parts, [enigma] * len(parts), [lock] * len(parts), [file] * len(parts), [log_lock] * len(parts), [log_file] * len(parts)))

        log_file.write("Logging ended...\n")

    # Объединение зашифрованных частей
    encrypted_text = ''.join(encrypted_parts)
    print("Исходный текст:", text)
    print("Зашифрованный текст:", encrypted_text)


if __name__ == "__main__":
    main()