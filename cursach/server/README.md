# Клиент-серверное приложение без Spring

Переписанный курсовой проект: бэкенд на чистой Java (встроенный `HttpServer` + SQLite + Gson) и тот же адаптивный фронтенд. Никаких Spring/Servlet контейнеров — только стандартная Java и простые зависимости.

## Возможности
- REST API для CRUD по сущности «событие» (`/api/events`).
- Хранение данных и истории запросов в SQLite.
- Одновременная работа множества клиентов через пул потоков.
- Подробное логирование входящих запросов в файл `logs/traffic.log` и таблицу `request_logs`.
- Конфигурационный файл `config/server_config.json` с путями БД, логов, статики и номером порта.
- Красивый веб-интерфейс (папка `server/public`) с фильтрами, поиском и формой редактирования.

## Структура проекта
```
cursach/
├── README.md
└── server/
    ├── build.gradle              # Gradle-проект без Spring
    ├── gradlew / gradlew.bat     # Wrapper-скрипты; Jar генерируется локально командой gradle wrapper
    ├── src/main/java/org/cursach/server/
    │   ├── App.java              # Точка входа, поднятие HTTP-сервера
    │   ├── config/ServerConfig.java
    │   ├── db/Database.java
    │   ├── http/EventHandler.java
    │   ├── http/StaticFileHandler.java
    │   ├── logging/RequestLogger.java
    │   └── model/Event.java
    ├── config/server_config.json
    ├── public/                   # фронтенд (index.html, styles.css, app.js, config.json)
    └── logs/, data/              # создаются при первом запуске
```

## Запуск
1. Перейдите в папку сервера:
   ```bash
   cd cursach/server
   ```
2. (Если файла `gradle/wrapper/gradle-wrapper.jar` нет) сгенерируйте его через установленный Gradle 8+:
   ```bash
   gradle wrapper --gradle-version 8.6
   ```
   Бинарный Jar не хранится в репозитории, чтобы избежать ошибок публикации. После генерации можно пользоваться `./gradlew`.
3. Проверка БД/конфига без запуска сервера (создаст таблицы):
   ```bash
   ./gradlew run --args="--healthcheck"
   ```
4. Запуск сервера на порте из `config/server_config.json` (по умолчанию 5000). При необходимости можно переопределить порт флагом `--port=XXXX`:
   ```bash
   ./gradlew run                          # порт из конфига
   ./gradlew run --args="--port=7000"    # принудительно 7000
   ```
5. Откройте браузер: `http://localhost:5000` (или другой порт из конфига).

> Если Gradle не установлен, скачайте его (8+) и выполните шаг 2 для генерации wrapper; после этого всё запускается через `./gradlew`.

## Работа с API
Базовый URL берётся из `public/config.json` (по умолчанию `/api`).

- Получить список событий: `GET /api/events`
- Создать событие: `POST /api/events` с JSON `{ "title": "...", "description": "...", "status": "planned|in_progress|done" }`
- Получить событие: `GET /api/events/{id}`
- Обновить событие: `PUT /api/events/{id}` (поля опциональны)
- Удалить событие: `DELETE /api/events/{id}`

Все запросы логируются в файл `logs/traffic.log` и таблицу `request_logs`, что упрощает анализ трафика (можно перехватывать и в Wireshark, т.к. протокол HTTP без шифрования).

## Кастомизация
- Измените порт, пути к БД/логам или каталог фронтенда в `config/server_config.json`; если порт занят, укажите другой или запустите с `--port=...`.
- Фронтенд конфиг (`public/config.json`) позволяет направить клиент на другой базовый URL API.

## Краткие заметки для защиты
- Используется встроенный `HttpServer` и JDBC-драйвер SQLite — без Spring, Tomcat или прочих контейнеров.
- Пул потоков (16 по умолчанию) обслуживает параллельные запросы.
- Схема БД: таблица `events` (данные клиента) и `request_logs` (логирование входящих запросов).
- Логи ведутся и в файлы, и в БД, что удовлетворяет требованиям по контролю трафика и сохранению клиентских данных.
