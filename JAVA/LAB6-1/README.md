# LAB6-1 — Клиент/сервер (TCP)

## Требования
- Java 17+
- Maven

## Структура
- `app/ServerMain` — запуск сервера
- `app/ClientMain` — запуск клиента
- `config/application.properties` — настройки клиента (IP/порт)

## Запуск сервера
Из корня проекта `JAVA/LAB6-1`:

```bash
mvn -q -DskipTests package
java -cp target/classes app.ServerMain 5000 devices.txt
```

Параметры:
- `5000` — порт (можно опустить, по умолчанию 5000)
- `devices.txt` — файл данных (можно опустить, по умолчанию `devices.txt`)

## Запуск клиента
В отдельном терминале:

```bash
mvn -q -DskipTests package
java -cp target/classes app.ClientMain config/application.properties
```

По умолчанию клиент читает `config/application.properties`:

```properties
server.host=127.0.0.1
server.port=5000
```

## Примечание по потокам
Сервер обрабатывает клиентов через `Executors.newCachedThreadPool()`.
