# LAB6: управление замками и сервером

Этот проект содержит два режима:

1. **Локальное консольное приложение** (`app.Main`)
   - Читает/пишет CSV `data/devices.csv` через `CsvDeviceRepository`.
   - В меню можно добавлять устройства, управлять ими, импортировать из `devices.txt`, менять файл импорта.
   - Все изменения сразу сохраняются в CSV.

2. **Сетевой режим**: TCP‑сервер + консольный клиент
   - Запуск сервера: `mvn -DskipTests exec:java -Dexec.mainClass=app.ServerMain` (порт в `config/server.properties`).
   - Запуск клиента: `mvn -DskipTests exec:java` (хост/порт в `config/application.properties`).

## Как редактировать данные на сервере
* Клиентские команды **3** (LOCK) и **4** (UNLOCK) отправляют на сервер запросы `LOCK <id>` / `UNLOCK <id>`.
* Сервер находит устройство по ID, меняет состояние и сразу сохраняет новый список в `data/devices.csv` — после этого LIST покажет обновлённые данные.
* Чтобы поменять CSV вручную, отредактируйте `data/devices.csv` на машине сервера и перезапустите сервер (или вызовите LIST, он перечитает файл).

## Быстрая проверка без Maven Exec
Если плагины Maven недоступны, собирайте и запускайте напрямую:
```bash
mvn -DskipTests compile   # заполнит target/classes
java -cp target/classes app.ServerMain
java -cp target/classes app.ClientMain
```

## Проверка соединения без клиента
При запущенном сервере можно отправить PING напрямую:
```bash
printf '\0\0\0\4PING' | nc 127.0.0.1 9000
```
Ответ должен быть `PONG`.
