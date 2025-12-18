# Клиент-серверное приложение без Spring

Полностью автономный курсовой проект: бэкенд на чистой Java (встроенный `HttpServer`, JDBC и SQLite), современный SPA-фронтенд и никакого Spring/Servlet-контейнера. Достаточно JDK 17+ и Gradle (wrapper можно сгенерировать самостоятельно, чтобы не хранить бинарный `gradle-wrapper.jar` в репозитории).

## Возможности
- REST API для CRUD по сущности «событие» (`/api/events`).
- Хранение данных и истории запросов в SQLite.
- Пул потоков обслуживает параллельные запросы.
- Подробное логирование входящего трафика в файл `logs/traffic.log` и таблицу `request_logs`.
- Конфигурация через `config/server_config.json` (порт, пути к БД/логам/статике).
- Красивый веб-интерфейс (папка `server/public`) с фильтрами, поиском и формой редактирования.

## Структура проекта
```
cursach/
├── README.md                 # этот файл
└── server/                   # Gradle-проект без Spring
    ├── build.gradle          # зависимости: Gson + sqlite-jdbc
    ├── gradlew / gradlew.bat # wrapper-скрипты (Jar необходимо сгенерировать локально)
    ├── src/main/java/org/cursach/server/
    │   ├── App.java          # точка входа и поднятие HttpServer
    │   ├── config/ServerConfig.java
    │   ├── db/Database.java
    │   ├── http/EventHandler.java
    │   ├── http/StaticFileHandler.java
    │   ├── logging/RequestLogger.java
    │   └── model/Event.java
    ├── config/server_config.json
    ├── public/               # фронтенд (index.html, styles.css, app.js, config.json)
    └── logs/, data/          # создаются при первом запуске
```

## Запуск
1. Перейдите в папку сервера:
   ```bash
   cd cursach/server
   ```
2. (Однократно) сгенерируйте wrapper Jar, если в репозитории его нет. Нужен установленный Gradle 8+:
   ```bash
   gradle wrapper --gradle-version 8.6
   ```
   После этого появится файл `gradle/wrapper/gradle-wrapper.jar` (он в .gitignore, т.к. считается бинарником) и можно работать через `./gradlew`.
3. Проверка конфигурации и БД без запуска сервера (создаёт таблицы):
   ```bash
   ./gradlew run --args="--healthcheck"
   ```
4. Запуск сервера на порте из `config/server_config.json` (по умолчанию 5000):
   ```bash
   ./gradlew run
   ```
5. Откройте браузер: `http://localhost:5000`.

## Работа с API
Базовый URL берётся из `public/config.json` (по умолчанию `/api`).

- Получить список событий: `GET /api/events`
- Создать событие: `POST /api/events` с JSON `{ "title": "...", "description": "...", "status": "planned|in_progress|done" }`
- Получить событие: `GET /api/events/{id}`
- Обновить событие: `PUT /api/events/{id}` (поля опциональны)
- Удалить событие: `DELETE /api/events/{id}`

Все запросы логируются в файл `logs/traffic.log` и таблицу `request_logs`, что упрощает анализ трафика (можно перехватывать и в Wireshark, т.к. протокол HTTP без шифрования).

## Кастомизация
- Измените порт, пути к БД/логам или каталог фронтенда в `config/server_config.json`.
- Фронтенд конфиг (`public/config.json`) позволяет направить клиент на другой базовый URL API.

## Краткие заметки для защиты
- Используется встроенный `HttpServer` и JDBC-драйвер SQLite — без Spring, Tomcat или внешних сервисов.
- Пул потоков обслуживает параллельные запросы, клиенты могут работать независимо.
- Схема БД: таблица `events` (данные клиента) и `request_logs` (логирование входящих запросов).
- Логи ведутся и в файлы, и в БД, что удовлетворяет требованиям по контролю трафика и сохранению клиентских данных.
