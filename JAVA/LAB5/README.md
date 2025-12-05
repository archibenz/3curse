# LAB5

Консольное приложение (MVC) для работы с устройствами блокировок и сохранением в CSV.

## Сборка и запуск
Каталог с исходниками именуется `LAB5` (в некоторых ОС регистр букв важен). В репозитории есть симлинк `java/lab5` → `JAVA/LAB5`, поэтому можно использовать оба написания.

Выполняйте команды из корня каталога `JAVA/LAB5`.

### Maven (pom.xml)
```bash
# сборка артефакта и ресурса devices.csv
mvn package

# запуск через exec-plugin
mvn exec:java
```

### Ручная сборка javac
```bash
mkdir -p build
javac $(find src -name "*.java") -d build
java -cp build app.Main
```

Если вы запускаете из корня всего репозитория, используйте относительный путь к исходникам (можно писать `JAVA/LAB5` или `java/lab5`):

```bash
(cd JAVA/LAB5 && mkdir -p build && javac $(find src -name "*.java") -d build && java -cp build app.Main)
```

Файл данных ожидается по пути `data/devices.csv` рядом с исходниками. Если приложение запускается из корня репозитория, путь `JAVA/LAB5/data/devices.csv` будет найден автоматически. При сборке через Maven содержимое каталога `data` попадает в `target/classes/data`.
